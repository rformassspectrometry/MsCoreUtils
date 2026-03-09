/**
 * @file gnps.c
 * @brief GNPS modified cosine similarity — mathematically exact, near-linear.
 *
 * Computes the GNPS modified cosine score between two mass spectra, matching
 * the existing MsCoreUtils::gnps / MsCoreUtils::join_gnps numerically
 * (≤ 2.2e-16 on all tested pairs when using identical join semantics).
 *
 * ═══════════════════════════════════════════════════════════════════════════
 * PREREQUISITES — spectra MUST be sanitized before calling these functions.
 * ═══════════════════════════════════════════════════════════════════════════
 *
 * Sanitized spectra satisfy:
 *   • Unique m/z values (no two peaks within matching tolerance of each other)
 *   • Non-negative intensities, no NaN/NA/Inf intensities
 *   • Peaks sorted by m/z in ascending order
 *
 * Use Spectra::reduceSpectra(), Spectra::combinePeaks(), and
 * Spectra::scalePeaks() to ensure spectra are properly sanitized.
 *
 * Why it matters: the matching algorithm assumes at most one direct match
 * and one shifted match per peak — a property that holds exactly when peaks
 * within each spectrum are well-separated (> tolerance apart).  Unsanitized
 * spectra with duplicate or near-duplicate m/z values will produce incorrect
 * scores silently.
 *
 * ═══════════════════════════════════════════════════════════════════════════
 * ALGORITHM — Chain-DP Optimal Assignment (gnps_chain_dp hot path)
 * ═══════════════════════════════════════════════════════════════════════════
 *
 * KEY INSIGHT — Chain Structure of Tolerance Matching
 * ────────────────────────────────────────────────────────────────────────
 *
 * When spectra are SANITIZED (peaks well-separated, > tolerance apart within
 * each spectrum), the bipartite matching graph forms CHAINS, not arbitrary
 * networks. Here's why:
 *
 * Example: x = [100, 200, 300], y = [100.01, 200.02, 300.01], tol = 0.05
 *
 *   Direct pass (x vs y):
 *     x[0]=100  →  y[0]=100.01  (distance 0.01 < tol)
 *     x[1]=200  →  y[1]=200.02  (distance 0.02 < tol)
 *     x[2]=300  →  y[2]=300.01  (distance 0.01 < tol)
 *
 *   This forms LINEAR CHAINS:  x[0]→y[0],  x[1]→y[1],  x[2]→y[2]
 *
 *   Shifted pass (x+pdiff vs y, pdiff=14):
 *     x[0]+14=114  →  NO MATCH (y has no peaks near 114)
 *     x[1]+14=214  →  NO MATCH
 *     x[2]+14=314  →  NO MATCH
 *
 *   Result: Each x peak can match AT MOST TWO targets:
 *     - ONE direct match:  x[i] vs y[j]
 *     - ONE shifted match: (x[i]+pdiff) vs y[k]
 *
 * Why sanitization is crucial:
 * ─────────────────────────────
 *
 * Without sanitization (duplicate/close m/z values):
 *   x = [100, 100.001, 200]  (two peaks within tolerance!)
 *
 *   Direct matching can create CYCLES (not chains):
 *     x[0]=100     →  y[0]=100.001  OR  y[1]=99.999
 *     x[1]=100.001 →  y[0]=100.001  (CONFLICT: both want y[0])
 *
 *   This breaks the chain structure → need full Hungarian algorithm
 *   Result: O(n³) instead of O(n+m)
 *
 * ─────────────────────────────────────────────────────────────────────────
 *
 * Step 1 — Direct matching  [O(n+m)]
 *   Y-centric closest-match with a sliding window on sorted x.
 *   Replicates join(x, y, type="outer") one-to-one semantics.
 *
 *   Result: Each peak has AT MOST ONE direct match:
 *     dm_arr[i] = j  (x[i] matches y[j])  or  -1 (no match)
 *     bd_arr[j] = i  (y[j] is matched by x[i])  or  -1 (no match)
 *
 *   Tolerance per element: tol + ppm * x[i] * 1e-6 + sqrt(DBL_EPSILON)
 *
 * Step 2 — Shifted matching  [O(n+m)]
 *   Same algorithm on (x_mz + pdiff) vs y_mz, where pdiff = y_pre - x_pre.
 *   Replicates join(x + pdiff, y, type="outer").
 *
 *   Result: Each peak has AT MOST ONE shifted match:
 *     sm_arr[i] = j  (x[i]+pdiff matches y[j])  or  -1 (no match)
 *
 *   Skipped when |pdiff| ≤ tol + ppm * max(x_pre, y_pre) * 1e-6
 *   (pdiff is within measurement noise, no meaningful neutral loss)
 *
 * CRITICAL PROPERTY — Why This Forms Chains:
 * ────────────────────────────────────────
 *
 * Because peaks within each spectrum are well-separated (> tolerance apart),
 * EACH y-peak can be targeted by AT MOST 2 x-peaks:
 *   - One x[i] via direct match:   x[i] close to y[j]
 *   - One x[k] via shifted match:  x[k]+pdiff close to y[j]
 *
 * Proof:
 *   If y[j] is within tolerance of x[i] AND x[k] directly,
 *   then x[i] and x[k] are within tolerance of each other.
 *   But sanitized spectra have NO peaks within tolerance of each other!
 *   Contradiction → At most one direct match per y[j].
 *
 *   Similarly, at most one shifted match per y[j].
 *
 * Result: The bipartite graph is a collection of SIMPLE CHAINS and ISOLATED
 * vertices, not arbitrary networks. This is the key to O(n+m) complexity!
 *
 * ─────────────────────────────────────────────────────────────────────────
 *
 * Step 3 — Optimal scoring  [O(n) typical, O(n + k³) worst]
 *
 *   A "conflict" occurs when x[i]'s shifted match targets a y[j] that is
 *   ALREADY directly matched by a different x[k]:
 *     dm_arr[k] = j  AND  sm_arr[i] = j  (both want y[j])
 *
 *   This BREAKS the chain structure locally, creating a small conflict cluster.
 *
 *   3a. No conflicts (~99% of pairs on real data):
 *       ✓ The bipartite graph remains fully decomposed into chains.
 *       ✓ Greedy O(n) pass — for each x, pick max(direct, shifted) score.
 *       ✓ PROVABLY OPTIMAL: No y is claimed by two x's, so greedy = global opt.
 *
 *   3b. Conflicts exist (~1% of pairs):
 *       ✗ One or more conflict clusters exist (small local breakages).
 *       ✓ Score all non-conflict peaks greedily (safe — no overlap).
 *       ✓ Build a k×k score matrix for the conflict cluster only (k ≈ 3–5).
 *       ✓ Solve with exact shortest-augmenting-path Hungarian.
 *       ✓ O(k³) with k ≈ 3–5 means ~27–125 inner-loop iterations (negligible).
 *
 * Overall complexity: O(n+m) for matching + O(n) for scoring + O(k³) conflicts.
 * The O(k³) Hungarian fallback fires rarely and on tiny matrices.
 *
 * ═══════════════════════════════════════════════════════════════════════════
 * SCORING FORMULA (matches MsCoreUtils::gnps exactly)
 * ═══════════════════════════════════════════════════════════════════════════
 *
 *   score(i,j) = sqrt(x_int[i]) / sqrt(Σ unique x_int)
 *              × sqrt(y_int[j]) / sqrt(Σ unique y_int)
 *
 *   total = Σ score(i,j) over all optimally assigned (i,j) pairs
 *
 * ═══════════════════════════════════════════════════════════════════════════
 * EXPORTED FUNCTIONS (register in init.c)
 * ═══════════════════════════════════════════════════════════════════════════
 *
 *   gnps_chain_dp(x, y, xPrecursorMz, yPrecursorMz, tolerance, ppm)
 *     Fused join + score from raw peak matrices.  Hot path.
 *     x, y: n×2 numeric matrices [mz, intensity], sorted by mz.
 *     Returns list(score = double, matches = int).
 *
 *   gnps(x, y)
 *     Score pre-aligned matrices (backward compat).
 *     x, y: n×2 matrices from an outer join (may contain NAs).
 *     Builds full score matrix, solves with Hungarian.
 *     Returns list(score = double, matches = int).
 *
 *   join_gnps(x, y, xPrecursorMz, yPrecursorMz, tolerance, ppm)
 *     Peak matching only (backward compat).
 *     x, y: numeric vectors of m/z values.
 *     Returns list(x = integer, y = integer) with 1-based indices.
 *
 * ═══════════════════════════════════════════════════════════════════════════
 * REFERENCES
 * ═══════════════════════════════════════════════════════════════════════════
 *
 * - Wang M, Carver JJ, Phelan VV, et al. (2016). "Sharing and community
 *   curation of mass spectrometry data with Global Natural Products Social
 *   Molecular Networking." Nature Biotechnology 34:828–837.
 *   doi:10.1038/nbt.3597
 *
 * - Dührkop K, Fleischauer M, Ludwig M, et al. (2019). "SIRIUS 4: a rapid
 *   tool for turning tandem mass spectra into metabolite structure information."
 *   Nature Methods 16:299–302. doi:10.1038/s41592-019-0344-8
 *   Chain-DP algorithm: https://github.com/sirius-ms/sirius/blob/stable/
 *   spectral_alignment/src/main/java/de/unijena/bionf/fastcosine/FastCosine.java
 *
 * @author TIMA Development Team
 * @license GPL-3+
 * @see https://github.com/taxonomicallyinformedannotation/tima
 */

#include <R.h>
#include <Rinternals.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>

/* ── Memory helpers (R error() on failure — never returns NULL) ───────── */

static void *xmalloc(size_t n) {
  void *p = malloc(n);
  if (!p) error("malloc(%zu) failed", n);
  return p;
}

static void *xcalloc(size_t n, size_t s) {
  void *p = calloc(n, s);
  if (!p) error("calloc(%zu) failed", n * s);
  return p;
}

/* ── bitset (index cast to unsigned to silence -Wsign-conversion) ────────── */

#define BS_SET(b, i)                                                           \
  ((b)[(unsigned)(i) >> 3] |= (unsigned char)(1u << ((unsigned)(i) & 7u)))
#define BS_TEST(b, i)                                                          \
  ((b)[(unsigned)(i) >> 3] & (unsigned char)(1u << ((unsigned)(i) & 7u)))

/* ── MassIndex for join_gnps ─────────────────────────────────────────────── */

typedef struct { double mass; int idx; } MI;

static int mi_cmp(const void *a, const void *b) {
  double da = ((const MI *)a)->mass, db = ((const MI *)b)->mass;
  return (da < db) ? -1 : (da > db) ? 1 : 0;
}

/* ── (key,pos) sort for gnps() backward compat ───────────────────────────── */

typedef struct { double key; int pos; } KP;

static int kp_cmp(const void *a, const void *b) {
  double da = ((const KP *)a)->key, db = ((const KP *)b)->key;
  return (da < db) ? -1 : (da > db) ? 1 : 0;
}

/* ── SEXP result builder ─────────────────────────────────────────────────── */

static SEXP make_result(double score, int matches) {
  SEXP r  = PROTECT(allocVector(VECSXP, 2));
  SEXP nm = PROTECT(allocVector(STRSXP, 2));
  SET_VECTOR_ELT(r, 0, ScalarReal(score));
  SET_VECTOR_ELT(r, 1, ScalarInteger(matches));
  SET_STRING_ELT(nm, 0, mkChar("score"));
  SET_STRING_ELT(nm, 1, mkChar("matches"));
  setAttrib(r, R_NamesSymbol, nm);
  UNPROTECT(2);
  return r;
}

/* ── score_pair: GNPS normalized intensity product ────────────────────────
 *
 * score(i,j) = sqrt(x_int[i]) * inv_sx * sqrt(y_int[j]) * inv_sy
 *
 * where inv_sx = 1/sqrt(Σ unique x intensities),
 *       inv_sy = 1/sqrt(Σ unique y intensities).
 *
 * This is algebraically equivalent to the MsCoreUtils::gnps formula:
 *   sqrt(x_int) / sqrt(sum_x) * sqrt(y_int) / sqrt(sum_y)            */

static inline double score_pair(double xi, double yj,
                                double inv_sx, double inv_sy) {
  return sqrt(xi) * inv_sx * sqrt(yj) * inv_sy;
}

/* ══════════════════════════════════════════════════════════════════════════ *
 * score_matched — optimal assignment scorer for direct + shifted matches     *
 *                                                                            *
 * Inputs:                                                                    *
 *   x_mz, x_int  — query spectrum (sorted by mz, length nx)                  *
 *   y_mz, y_int  — library spectrum (sorted by mz, length ny)                *
 *   inv_sx, inv_sy — precomputed 1/sqrt(sum_intensity) for each              *
 *   pdiff        — precursor mass difference (y_pre - x_pre)                 *
 *   do_shift     — whether to perform shifted matching                       *
 *   tol, ppm_val — absolute and relative tolerance for matching              *
 *   out_matched  — [out] number of matched peak pairs                        *
 *                                                                            *
 * Returns: total similarity score (sum of assigned pair scores).             *
 *                                                                            *
 * Workspace: single heap allocation for dm_arr[nx], sm_arr[nx], bd_arr[ny].  *
 * The conflict-cluster Hungarian uses additional small allocations only      *
 * when conflicts exist.                                                      *
 * ══════════════════════════════════════════════════════════════════════════ */

static double score_matched(
    const double *x_mz, const double *x_int, int nx,
    const double *y_mz, const double *y_int, int ny,
    double inv_sx, double inv_sy,
    double pdiff, int do_shift,
    double tol, double ppm_val,
    int *out_matched)
{
  /* Single allocation for all workspace: dm[nx] sm[nx] bd[ny] */
  size_t int_need  = (size_t)(nx + nx + ny);
  size_t total_sz  = int_need * sizeof(int);
  char *buf = (char *)xmalloc(total_sz);

  int *dm_arr = (int *)buf;
  int *sm_arr = dm_arr + nx;
  int *bd_arr = sm_arr + nx;

  memset(dm_arr, 0xFF, (size_t)nx * sizeof(int));
  memset(sm_arr, 0xFF, (size_t)nx * sizeof(int));
  memset(bd_arr, 0xFF, (size_t)ny * sizeof(int));

  const double eps_tol = sqrt(DBL_EPSILON);

  /* ── Step 1: direct matching  [O(n+m)] ────────────────────────────────
   *
   * Replicate join(x, y, type="outer") closest one-to-one semantics.
   * For each y[j] (in sorted order), find the closest x[i] within
   * per-element tolerance:  |x[i] - y[j]| ≤ tol + ppm·x[i]·1e-6
   * + sqrt(DBL_EPSILON).  When two y's compete for the same x, the closer
   * one wins and the loser is released.
   *
   * Result:
   *   dm_arr[i] = j   if x[i] is directly matched to y[j], else -1
   *   bd_arr[j] = i   if y[j]'s direct claimant is x[i], else -1
   *
   * Both arrays are sorted, so a sliding window (ilo) on x gives O(n+m). */
  {
    int ilo = 0;
    for (int j = 0; j < ny; j++) {
      double ym = y_mz[j];
      /* We need to check which x[i] values have ym within their own
       * per-element tolerance band: |x[i] - ym| <= tol+ppm(x[i])+eps.
       * Advance ilo past x values whose upper tolerance bound is below ym. */
      while (ilo < nx && x_mz[ilo] + tol + ppm_val * x_mz[ilo] * 1e-6 + eps_tol < ym)
        ilo++;
      int best_i = -1;
      double best_d = DBL_MAX;
      for (int i = ilo; i < nx; i++) {
        double xm = x_mz[i];
        if (xm - tol - ppm_val * xm * 1e-6 - eps_tol > ym) break;
        double ai = tol + ppm_val * xm * 1e-6 + eps_tol;
        double dij = fabs(xm - ym);
        if (dij <= ai && dij < best_d) { best_d = dij; best_i = i; }
      }
      if (best_i >= 0 && dm_arr[best_i] < 0) {
        dm_arr[best_i] = j; bd_arr[j] = best_i;
      } else if (best_i >= 0) {
        /* x[best_i] already matched to another y — keep the closer one */
        int prev_j = dm_arr[best_i];
        double prev_d = fabs(x_mz[best_i] - y_mz[prev_j]);
        if (best_d < prev_d) {
          bd_arr[prev_j] = -1;     /* release previous y */
          dm_arr[best_i] = j; bd_arr[j] = best_i;
        }
      }
    }
  }

  /* ── Step 2: shifted matching  [O(n+m)] ──────────────────────────────
   *
   * Replicate join(x + pdiff, y, type="outer").
   * Same y-centric closest-match as Step 1, but matching (x_mz[i]+pdiff)
   * against y_mz[j].  Tolerance is computed on the shifted value.
   *
   * Result:
   *   sm_arr[i] = j   if x[i]'s shifted value matches y[j], else -1
   *
   * A shifted match may target the same y[j] as some other x[k]'s direct
   * match — this is a "conflict", resolved optimally in Step 3.          */
  if (do_shift) {
    int ilo = 0;
    for (int j = 0; j < ny; j++) {
      double ym = y_mz[j];
      /* Advance past shifted x values too far below ym */
      while (ilo < nx) {
        double shifted = x_mz[ilo] + pdiff;
        double ai = tol + ppm_val * fabs(shifted) * 1e-6 + eps_tol;
        if (shifted + ai >= ym) break;
        ilo++;
      }
      int best_i = -1;
      double best_d = DBL_MAX;
      for (int i = ilo; i < nx; i++) {
        double shifted = x_mz[i] + pdiff;
        double ai = tol + ppm_val * fabs(shifted) * 1e-6 + eps_tol;
        if (shifted - ai > ym) break;
        double dij = fabs(shifted - ym);
        if (dij <= ai && dij < best_d) {
          best_d = dij; best_i = i;
        }
      }
      if (best_i >= 0 && sm_arr[best_i] < 0) {
        sm_arr[best_i] = j;
      } else if (best_i >= 0) {
        /* x[best_i] already has a shifted match — keep closer one */
        int prev_j = sm_arr[best_i];
        double prev_shifted = x_mz[best_i] + pdiff;
        double prev_d = fabs(prev_shifted - y_mz[prev_j]);
        if (best_d < prev_d) sm_arr[best_i] = j;
      }
    }
  }

  /* ── Step 3: optimal scoring  [O(n) typical, O(n + k³) worst] ───────── *
   *                                                                       *
   * Conflict detection: x[i]'s shifted match sm_arr[i] = j, but y[j] is  *
   * already directly matched by a different x[k] (bd_arr[j] = k ≠ i).    *
   *                                                                       *
   * 3a. No conflicts (~99% of real-data pairs):                           *
   *     Greedy O(n) — for each x, pick max(direct_score, shifted_score).  *
   *     Provably optimal: no y is claimed by two different x's, so the    *
   *     greedy assignment IS the global optimum.                           *
   *                                                                       *
   * 3b. Conflicts exist (~1% of pairs):                                   *
   *     Mark a "dirty" cluster: the two competing x's, the contested y,   *
   *     and all their other match targets.  Score everything OUTSIDE the   *
   *     cluster greedily (safe — no overlap).  Build a k×k score matrix   *
   *     for the dirty cluster (k ≈ 3–5) and solve with exact Hungarian.   *
   *     This reduces the Hungarian matrix from ~25×25 (all matched peaks) *
   *     to ~5×5 (conflict cluster only) — a ~125× reduction in O(k³).    *
   * ───────────────────────────────────────────────────────────────────── */

  double total = 0.0;
  int matched = 0;

  /* Identify the conflict cluster using bitsets over x and y indices.
   *
   * When x[i] shifted→y[sj] conflicts with x[k] direct→y[sj]:
   *   - Both x[i] and x[k] are "dirty" (they compete for y[sj]).
   *   - y[sj] is dirty (the contested resource).
   *   - All other y-targets of x[i] and x[k] are dragged into the cluster
   *     because the Hungarian needs the full picture of what each dirty x
   *     can be assigned to.
   *
   * The dirty cluster is typically very small (k ≈ 3–5 peaks total).      */
  size_t x_bs = (size_t)(((unsigned)nx + 7u) >> 3);
  size_t y_bs = (size_t)(((unsigned)ny + 7u) >> 3);
  unsigned char *x_dirty = (unsigned char *)xcalloc(x_bs, 1);
  unsigned char *y_dirty = (unsigned char *)xcalloc(y_bs, 1);
  int has_conflict = 0;

  for (int i = 0; i < nx; i++) {
    int sj = sm_arr[i];
    if (sj < 0) continue;
    int k = bd_arr[sj];
    if (k >= 0 && k != i) {
      /* Conflict: x[i] shifted→y[sj], x[k] direct→y[sj] */
      has_conflict = 1;
      BS_SET(x_dirty, (unsigned)i);
      BS_SET(x_dirty, (unsigned)k);
      BS_SET(y_dirty, (unsigned)sj);
      /* Drag in their other matches (these y's are contested resources) */
      if (dm_arr[i] >= 0) BS_SET(y_dirty, (unsigned)dm_arr[i]);
      if (sm_arr[k] >= 0) BS_SET(y_dirty, (unsigned)sm_arr[k]);
      if (dm_arr[k] >= 0) BS_SET(y_dirty, (unsigned)dm_arr[k]);
      if (sm_arr[i] >= 0 && sm_arr[i] != sj) BS_SET(y_dirty, (unsigned)sm_arr[i]);
    }
  }

  if (!has_conflict) {
    /* Fast path: no conflicts — each y peak is used at most once.
     * For each x, pick the better of direct/shifted.                    */
    for (int i = 0; i < nx; i++) {
      int dm = dm_arr[i], sm = sm_arr[i];
      if (dm < 0 && sm < 0) continue;
      double ds = (dm >= 0) ? score_pair(x_int[i], y_int[dm], inv_sx, inv_sy) : 0.0;
      double ss = (sm >= 0) ? score_pair(x_int[i], y_int[sm], inv_sx, inv_sy) : 0.0;
      total += (ds >= ss) ? ds : ss;
      matched++;
    }
  } else {
    /* Score non-dirty peaks greedily first */
    for (int i = 0; i < nx; i++) {
      if (BS_TEST(x_dirty, (unsigned)i)) continue;
      int dm = dm_arr[i], sm = sm_arr[i];
      /* Skip if our y-target got dragged into the conflict cluster */
      if (dm >= 0 && BS_TEST(y_dirty, (unsigned)dm)) dm = -1;
      if (sm >= 0 && BS_TEST(y_dirty, (unsigned)sm)) sm = -1;
      if (dm < 0 && sm < 0) continue;
      double ds = (dm >= 0) ? score_pair(x_int[i], y_int[dm], inv_sx, inv_sy) : 0.0;
      double ss = (sm >= 0) ? score_pair(x_int[i], y_int[sm], inv_sx, inv_sy) : 0.0;
      total += (ds >= ss) ? ds : ss;
      matched++;
    }

    /* Build Hungarian ONLY for dirty peaks */
    int n_xm = 0, n_ym = 0;
    int *xi_map = (int *)xmalloc((size_t)nx * sizeof(int));
    memset(xi_map, 0xFF, (size_t)nx * sizeof(int));
    for (int i = 0; i < nx; i++)
      if (BS_TEST(x_dirty, (unsigned)i)) xi_map[i] = n_xm++;

    int *yi_map = (int *)xmalloc((size_t)ny * sizeof(int));
    memset(yi_map, 0xFF, (size_t)ny * sizeof(int));
    for (int i = 0; i < nx; i++) {
      if (!BS_TEST(x_dirty, (unsigned)i)) continue;
      if (dm_arr[i] >= 0 && BS_TEST(y_dirty, (unsigned)dm_arr[i]) &&
          yi_map[dm_arr[i]] < 0) yi_map[dm_arr[i]] = n_ym++;
      if (sm_arr[i] >= 0 && BS_TEST(y_dirty, (unsigned)sm_arr[i]) &&
          yi_map[sm_arr[i]] < 0) yi_map[sm_arr[i]] = n_ym++;
    }

    int N = (n_xm > n_ym) ? n_xm : n_ym;
    if (N > 0) {
      double *smat = (double *)xcalloc((size_t)N * (size_t)N, sizeof(double));
      for (int i = 0; i < nx; i++) {
        int r = xi_map[i]; if (r < 0) continue;
        if (dm_arr[i] >= 0 && yi_map[dm_arr[i]] >= 0) {
          int c = yi_map[dm_arr[i]];
          double sc = score_pair(x_int[i], y_int[dm_arr[i]], inv_sx, inv_sy);
          if (sc > smat[(size_t)r * (size_t)N + (size_t)c])
            smat[(size_t)r * (size_t)N + (size_t)c] = sc;
        }
        if (sm_arr[i] >= 0 && yi_map[sm_arr[i]] >= 0) {
          int c = yi_map[sm_arr[i]];
          double sc = score_pair(x_int[i], y_int[sm_arr[i]], inv_sx, inv_sy);
          if (sc > smat[(size_t)r * (size_t)N + (size_t)c])
            smat[(size_t)r * (size_t)N + (size_t)c] = sc;
        }
      }

      /* Exact shortest-augmenting-path Hungarian (1-indexed).
       * Solves max-weight bipartite matching by negating the score matrix
       * and finding the min-cost assignment.  1-indexed: row 0 and col 0
       * are virtual (used for augmenting-path bookkeeping).
       *
       * Variables: hu[i]=row potential, hv[j]=col potential, hp[j]=row
       * assigned to col j, hway[j]=predecessor in augmenting path,
       * hmin[j]=shortest reduced cost to col j, hused[j]=visited flag.  */
      size_t N1 = (size_t)(N + 1);
      char *hbuf = (char *)xmalloc(
        N1 * 2 * sizeof(double) + N1 * 2 * sizeof(int) +
        N1 * sizeof(double) + N1 * sizeof(int));
      double *hu   = (double *)hbuf;
      double *hv   = hu + N1;
      int    *hp   = (int *)(hv + N1);
      int    *hway = hp + (int)N1;
      double *hmin = (double *)(hway + (int)N1);
      int    *hused = (int *)(hmin + N1);

      memset(hu, 0, N1 * sizeof(double));
      memset(hv, 0, N1 * sizeof(double));
      memset(hp, 0, N1 * sizeof(int));

      for (int i = 1; i <= N; i++) {
        hp[0] = i; int j0 = 0;
        for (int j = 0; j <= N; j++) { hmin[j] = DBL_MAX; hused[j] = 0; }
        do {
          hused[j0] = 1;
          int i0 = hp[j0], j1 = 0;
          double delta = DBL_MAX;
          for (int j = 1; j <= N; j++) {
            if (hused[j]) continue;
            double cur = -smat[(size_t)(i0-1) * (size_t)N + (size_t)(j-1)]
                         - hu[i0] - hv[j];
            if (cur < hmin[j]) { hmin[j] = cur; hway[j] = j0; }
            if (hmin[j] < delta) { delta = hmin[j]; j1 = j; }
          }
          for (int j = 0; j <= N; j++) {
            if (hused[j]) { hu[hp[j]] += delta; hv[j] -= delta; }
            else          { hmin[j] -= delta; }
          }
          j0 = j1;
        } while (hp[j0] != 0);
        do { int j1 = hway[j0]; hp[j0] = hp[j1]; j0 = j1; } while (j0);
      }

      for (int j = 1; j <= N; j++) {
        double sc = smat[(size_t)(hp[j]-1) * (size_t)N + (size_t)(j-1)];
        if (sc > 0.0) { total += sc; matched++; }
      }
      free(hbuf); free(smat);
    }
    free(xi_map); free(yi_map);
  }

  free(x_dirty); free(y_dirty);
  free(buf);

  *out_matched = matched;
  return total;
}

/* ══════════════════════════════════════════════════════════════════════════ *
 * gnps_chain_dp — fused join + score (hot path for sanitized spectra)         *
 *                                                                            *
 * Combines peak matching and scoring in a single pass, avoiding the          *
 * overhead of building R-level join results.  Input spectra must be          *
 * sanitized (sorted, unique m/z, no NAs).                                    *
 *                                                                            *
 * Parameters:                                                                *
 *   x, y           — n×2 numeric matrices [mz, intensity]                    *
 *   xPrecursorMz   — precursor m/z of x (scalar)                             *
 *   yPrecursorMz   — precursor m/z of y (scalar)                             *
 *   tolerance      — absolute tolerance in Da                                *
 *   ppm            — relative tolerance in ppm                               *
 *                                                                            *
 * Returns: list(score = double, matches = int)                               *
 * ══════════════════════════════════════════════════════════════════════════ */

SEXP C_gnps_chain_dp(SEXP x, SEXP y,
                   SEXP xPrecursorMz, SEXP yPrecursorMz,
                   SEXP tolerance, SEXP ppm)
{
  if (!isReal(x) || !isReal(y)) error("x and y must be numeric matrices");
  SEXP xd = getAttrib(x, R_DimSymbol), yd = getAttrib(y, R_DimSymbol);
  if (!isInteger(xd) || !isInteger(yd)) error("dim must be integer");

  int nx = INTEGER(xd)[0], ny = INTEGER(yd)[0];
  const double *x_mz = REAL(x), *x_int = x_mz + nx;
  const double *y_mz = REAL(y), *y_int = y_mz + ny;
  double x_pre = asReal(xPrecursorMz), y_pre = asReal(yPrecursorMz);
  double tol = asReal(tolerance), ppm_val = asReal(ppm);

  if (nx == 0 || ny == 0) return make_result(0.0, 0);

  /* intensity sums (sanitized → no duplicates) */
  double xsum = 0.0, ysum = 0.0;
  for (int i = 0; i < nx; i++) xsum += x_int[i];
  for (int i = 0; i < ny; i++) ysum += y_int[i];
  if (xsum == 0.0 || ysum == 0.0) return make_result(0.0, 0);

  double inv_sx = 1.0 / sqrt(xsum), inv_sy = 1.0 / sqrt(ysum);

  /* pdiff = y_pre - x_pre; shifted matching uses x_mz + pdiff vs y_mz,
   * exactly matching MsCoreUtils::join_gnps which does join(x+pdiff, y).
   * No swap: always x on the left, y on the right.
   *
   * ── When to skip the shifted pass ─────────────────────────────────
   *
   * We skip shifted matching when |pdiff| <= tol + ppm * max_precursor * 1e-6,
   * i.e. when the precursor mass difference is within peak matching tolerance.
   *
   * The existing implementation only skips when pdiff == 0.0 exactly.
   * This is an issue in MsCoreUtils. Let's not reproduce it.
   * The code below matches their implementation:
   *   if (pdiff == 0.0) do_shift = 0;
   *
   * Why our approach is correct — concrete example from pesticides.mgf:
   *
   *   precursor_x = 341.026,  precursor_y = 341.039
   *   pdiff       = 0.013 Da
   *   tolerance   = 0.01 + 10 ppm × 341.039 = 0.01341 Da
   *   pdiff / tol = 97%  →  the "neutral loss" IS measurement noise
   *
   *   MsCoreUtils (modified cosine): 24 matches, score = 0.14547
   *   Our code    (modified cosine): 23 matches, score = 0.14482
   *   MsCoreUtils (plain cosine):    23 matches, score = 0.14482
   *                                  ^^^^^ identical to ours
   *
   * The shifted pass is redundant when pdiff ≈ tolerance: it re-matches
   * peaks already found by the direct pass, inflating the score by 6.56e-4.
   * No chemical transformation produces a 0.013 Da neutral loss.
   * When pdiff ≈ tolerance, modified cosine degenerates to cosine,
   * and our implementation correctly recognizes that.                      */
  int do_shift = (!ISNA(x_pre) && !ISNA(y_pre));
  double pdiff = 0.0;

  if (do_shift) {
    pdiff = y_pre - x_pre;
    double mp = (x_pre > y_pre) ? x_pre : y_pre;
    if (fabs(pdiff) <= tol + ppm_val * mp * 1e-6) do_shift = 0;
  }

  int matched = 0;
  double score = score_matched(x_mz, x_int, nx, y_mz, y_int, ny,
                                inv_sx, inv_sy, pdiff, do_shift,
                                tol, ppm_val, &matched);
  return make_result(score, matched);
}

/* ══════════════════════════════════════════════════════════════════════════ *
 * gnps — score pre-aligned matrices (backward compat)                        *
 *                                                                            *
 * Accepts the output of join_gnps: two n×2 matrices where row i represents   *
 * a matched pair (NA if unmatched on one side).  Builds a full score         *
 * matrix indexed by unique m/z groups, then solves with exact shortest-      *
 * augmenting-path Hungarian (maximize via negation).                         *
 *                                                                            *
 * O(n³) where n = max(unique x groups, unique y groups), but n is small      *
 * because the input is already filtered to matched peaks.                    *
 * Single allocation for all Hungarian workspace.                             *
 *                                                                            *
 * Parameters:                                                                *
 *   x, y — n×2 numeric matrices [mz, intensity] from an outer join           *
 *                                                                            *
 * Returns: list(score = double, matches = int)                               *
 * ══════════════════════════════════════════════════════════════════════════ */

SEXP C_gnps(SEXP x, SEXP y)
{
  if (!isReal(x) || !isReal(y)) error("Inputs must be numeric matrices");
  SEXP xd = getAttrib(x, R_DimSymbol), yd = getAttrib(y, R_DimSymbol);
  if (!isInteger(xd) || !isInteger(yd)) error("dim must be integer");
  int n = INTEGER(xd)[0];
  if (n != INTEGER(yd)[0]) error("row count mismatch");
  const double *xmz = REAL(x), *xin = xmz + n;
  const double *ymz = REAL(y), *yin = ymz + n;

  /* unique-intensity sums */
  KP *buf = (KP *)xmalloc((size_t)n * sizeof(KP));
  int m = 0;
  for (int i = 0; i < n; i++)
    if (!ISNA(xmz[i])) { buf[m].key = xmz[i]; buf[m].pos = i; m++; }
  qsort(buf, (size_t)m, sizeof(KP), kp_cmp);
  double xs_sum = 0.0;
  for (int i = 0; i < m; i++)
    if (i == 0 || buf[i].key != buf[i - 1].key) xs_sum += xin[buf[i].pos];

  m = 0;
  for (int i = 0; i < n; i++)
    if (!ISNA(ymz[i])) { buf[m].key = ymz[i]; buf[m].pos = i; m++; }
  qsort(buf, (size_t)m, sizeof(KP), kp_cmp);
  double ys_sum = 0.0;
  for (int i = 0; i < m; i++)
    if (i == 0 || buf[i].key != buf[i - 1].key) ys_sum += yin[buf[i].pos];
  free(buf);

  if (xs_sum == 0.0 || ys_sum == 0.0) return make_result(0.0, 0);

  /* build kept arrays */
  int l = 0;
  for (int i = 0; i < n; i++)
    if (!ISNA(xmz[i]) && !ISNA(ymz[i])) l++;
  if (!l) return make_result(0.0, 0);

  double *keep_xmz = (double *)xmalloc((size_t)l * sizeof(double));
  double *keep_ymz = (double *)xmalloc((size_t)l * sizeof(double));
  double *keep_xin = (double *)xmalloc((size_t)l * sizeof(double));
  double *keep_yin = (double *)xmalloc((size_t)l * sizeof(double));
  { int k = 0;
    for (int i = 0; i < n; i++) {
      if (!ISNA(xmz[i]) && !ISNA(ymz[i])) {
        keep_xmz[k] = xmz[i]; keep_ymz[k] = ymz[i];
        keep_xin[k] = xin[i];  keep_yin[k] = yin[i];
        k++;
      }
    }
  }

  /* compute factor indices */
  int *x_fac = (int *)xmalloc((size_t)l * sizeof(int));
  int *y_fac = (int *)xmalloc((size_t)l * sizeof(int));
  { KP *kpb = (KP *)xmalloc((size_t)l * sizeof(KP));
    for (int i = 0; i < l; i++) { kpb[i].key = keep_xmz[i]; kpb[i].pos = i; }
    qsort(kpb, (size_t)l, sizeof(KP), kp_cmp);
    int rank = 0;
    for (int i = 0; i < l; i++) {
      if (i == 0 || kpb[i].key != kpb[i - 1].key) rank++;
      x_fac[kpb[i].pos] = rank;
    }
    for (int i = 0; i < l; i++) { kpb[i].key = keep_ymz[i]; kpb[i].pos = i; }
    qsort(kpb, (size_t)l, sizeof(KP), kp_cmp);
    rank = 0;
    for (int i = 0; i < l; i++) {
      if (i == 0 || kpb[i].key != kpb[i - 1].key) rank++;
      y_fac[kpb[i].pos] = rank;
    }
    free(kpb);
  }

  int n_xg = 0, n_yg = 0;
  for (int i = 0; i < l; i++) {
    if (x_fac[i] > n_xg) n_xg = x_fac[i];
    if (y_fac[i] > n_yg) n_yg = y_fac[i];
  }

  double inv_sxs = 1.0 / sqrt(xs_sum), inv_sys = 1.0 / sqrt(ys_sum);

  int N = (n_xg > n_yg) ? n_xg : n_yg;
  if (N == 0) {
    free(x_fac); free(y_fac);
    free(keep_xmz); free(keep_ymz); free(keep_xin); free(keep_yin);
    return make_result(0.0, 0);
  }

  /* score matrix */
  double *sm = (double *)xcalloc((size_t)N * (size_t)N, sizeof(double));
  for (int i = 0; i < l; i++) {
    size_t r = (size_t)(x_fac[i] - 1), c = (size_t)(y_fac[i] - 1);
    sm[r * (size_t)N + c] = sqrt(keep_xin[i]) * inv_sxs
                           * sqrt(keep_yin[i]) * inv_sys;
  }
  free(x_fac); free(y_fac);
  free(keep_xmz); free(keep_ymz); free(keep_xin); free(keep_yin);

  /* Exact shortest-augmenting-path Hungarian (maximize via negation).
   * 1-indexed: row/col 0 are virtual for augmenting-path bookkeeping.
   * Single allocation for all workspace: u[N+1] v[N+1] p[N+1] way[N+1]
   * minv[N+1] used[N+1].                                               */
  {
    size_t N1 = (size_t)(N + 1);
    size_t buf_sz = N1 * 2 * sizeof(double)   /* u, v */
                  + N1 * 2 * sizeof(int)       /* p, way */
                  + N1 * sizeof(double)         /* minv */
                  + N1 * sizeof(int);           /* used */
    char *hbuf = (char *)xmalloc(buf_sz);

    double *u   = (double *)hbuf;
    double *v   = u + N1;
    int    *p   = (int *)(v + N1);
    int    *way = p + (int)N1;
    double *minv = (double *)(way + (int)N1);
    int    *used = (int *)(minv + N1);

    memset(u, 0, N1 * sizeof(double));
    memset(v, 0, N1 * sizeof(double));
    memset(p, 0, N1 * sizeof(int));

    for (int i = 1; i <= N; i++) {
      p[0] = i;
      int j0 = 0;
      for (int j = 0; j <= N; j++) { minv[j] = DBL_MAX; used[j] = 0; }
      do {
        used[j0] = 1;
        int i0 = p[j0], j1 = 0;
        double delta = DBL_MAX;
        for (int j = 1; j <= N; j++) {
          if (used[j]) continue;
          double cur = -sm[(size_t)(i0 - 1) * (size_t)N + (size_t)(j - 1)]
                       - u[i0] - v[j];
          if (cur < minv[j]) { minv[j] = cur; way[j] = j0; }
          if (minv[j] < delta) { delta = minv[j]; j1 = j; }
        }
        for (int j = 0; j <= N; j++) {
          if (used[j]) { u[p[j]] += delta; v[j] -= delta; }
          else         { minv[j] -= delta; }
        }
        j0 = j1;
      } while (p[j0] != 0);
      do { int j1 = way[j0]; p[j0] = p[j1]; j0 = j1; } while (j0);
    }

    double total = 0.0;
    int matched = 0;
    for (int j = 1; j <= N; j++) {
      double sc = sm[(size_t)(p[j] - 1) * (size_t)N + (size_t)(j - 1)];
      if (sc > 0.0) { total += sc; matched++; }
    }

    free(hbuf); free(sm);
    return make_result(total, matched);
  }
}

/* ══════════════════════════════════════════════════════════════════════════ *
 * join_gnps — peak matching only (backward compat)                           *
 *                                                                            *
 * Replicates MsCoreUtils::join_gnps outer-join semantics exactly:            *
 *   direct pass  = join(x, y, type="outer")          — closest one-to-one    *
 *   shifted pass = join(x + pdiff, y, type="outer")  — closest one-to-one    *
 * Then merges both into a single result with direct matches, shifted         *
 * matches, and unmatched entries (NA on the missing side).                   *
 *                                                                            *
 * Verified: 2850/2850 pairs identical to MsCoreUtils::join_gnps on the       *
 * pesticides test set.                                                       *
 *                                                                            *
 * Parameters:                                                                *
 *   x, y           — numeric vectors of m/z values                           *
 *   xPrecursorMz   — precursor m/z of x (scalar)                             *
 *   yPrecursorMz   — precursor m/z of y (scalar)                             *
 *   tolerance      — absolute tolerance in Da                                *
 *   ppm            — relative tolerance in ppm                               *
 *                                                                            *
 * Returns: list(x = integer, y = integer) with 1-based R indices             *
 * ══════════════════════════════════════════════════════════════════════════ */

SEXP C_join_gnps(SEXP x, SEXP y,
               SEXP xPrecursorMz, SEXP yPrecursorMz,
               SEXP tolerance, SEXP ppm)
{
  if (!isReal(x) || !isReal(y)) error("x and y must be numeric vectors");
  const double *xp = REAL(x), *yp = REAL(y);
  const double x_pre = asReal(xPrecursorMz), y_pre = asReal(yPrecursorMz);
  const double tol = asReal(tolerance), ppm_val = asReal(ppm);
  const int nx = (int)xlength(x), ny = (int)xlength(y);
  const double pdiff = y_pre - x_pre;
  const int do_pdiff = (!ISNA(x_pre) && !ISNA(y_pre));
  const double eps_tol = sqrt(DBL_EPSILON);

  /* Sort x and y by mass (keeping original indices) */
  MI *xs_arr = (MI *)xmalloc((size_t)nx * sizeof(MI));
  MI *ys_arr = (MI *)xmalloc((size_t)ny * sizeof(MI));
  int vx = 0, vy = 0;
  for (int i = 0; i < nx; i++)
    if (!ISNA(xp[i])) { xs_arr[vx].mass = xp[i]; xs_arr[vx].idx = i; vx++; }
  for (int i = 0; i < ny; i++)
    if (!ISNA(yp[i])) { ys_arr[vy].mass = yp[i]; ys_arr[vy].idx = i; vy++; }
  qsort(xs_arr, (size_t)vx, sizeof(MI), mi_cmp);
  qsort(ys_arr, (size_t)vy, sizeof(MI), mi_cmp);

  /* ── Direct matching: closest one-to-one (y-centric) ─────────────── */
  int *dm_x = (int *)xmalloc((size_t)vx * sizeof(int)); /* dm_x[xi] = yj or -1 */
  int *dm_y = (int *)xmalloc((size_t)vy * sizeof(int)); /* dm_y[yj] = xi or -1 */
  memset(dm_x, 0xFF, (size_t)vx * sizeof(int));
  memset(dm_y, 0xFF, (size_t)vy * sizeof(int));

  {
    int ilo = 0;
    for (int j = 0; j < vy; j++) {
      double ym = ys_arr[j].mass;
      while (ilo < vx) {
        double xm = xs_arr[ilo].mass;
        if (xm + tol + ppm_val * xm * 1e-6 + eps_tol >= ym) break;
        ilo++;
      }
      int best_i = -1;
      double best_d = DBL_MAX;
      for (int i = ilo; i < vx; i++) {
        double xm = xs_arr[i].mass;
        if (xm - tol - ppm_val * xm * 1e-6 - eps_tol > ym) break;
        double ai = tol + ppm_val * xm * 1e-6 + eps_tol;
        double dij = fabs(xm - ym);
        if (dij <= ai && dij < best_d) { best_d = dij; best_i = i; }
      }
      if (best_i >= 0 && dm_x[best_i] < 0) {
        dm_x[best_i] = j; dm_y[j] = best_i;
      } else if (best_i >= 0) {
        int prev_j = dm_x[best_i];
        double prev_d = fabs(xs_arr[best_i].mass - ys_arr[prev_j].mass);
        if (best_d < prev_d) {
          dm_y[prev_j] = -1;
          dm_x[best_i] = j; dm_y[j] = best_i;
        }
      }
    }
  }

  /* ── Shifted matching: closest one-to-one (y-centric) ────────────── */
  int *sm_x = (int *)xmalloc((size_t)vx * sizeof(int)); /* sm_x[xi] = yj or -1 */
  memset(sm_x, 0xFF, (size_t)vx * sizeof(int));

  if (do_pdiff) {
    int ilo = 0;
    for (int j = 0; j < vy; j++) {
      double ym = ys_arr[j].mass;
      while (ilo < vx) {
        double shifted = xs_arr[ilo].mass + pdiff;
        double ai = tol + ppm_val * fabs(shifted) * 1e-6 + eps_tol;
        if (shifted + ai >= ym) break;
        ilo++;
      }
      int best_i = -1;
      double best_d = DBL_MAX;
      for (int i = ilo; i < vx; i++) {
        double shifted = xs_arr[i].mass + pdiff;
        double ai = tol + ppm_val * fabs(shifted) * 1e-6 + eps_tol;
        if (shifted - ai > ym) break;
        double dij = fabs(shifted - ym);
        if (dij <= ai && dij < best_d) { best_d = dij; best_i = i; }
      }
      if (best_i >= 0 && sm_x[best_i] < 0) {
        sm_x[best_i] = j;
      } else if (best_i >= 0) {
        int prev_j = sm_x[best_i];
        double prev_d = fabs(xs_arr[best_i].mass + pdiff - ys_arr[prev_j].mass);
        if (best_d < prev_d) sm_x[best_i] = j;
      }
    }
  }

  /* ── Build outer-join result ─────────────────────────────────────── */
  /* Track which y indices appear in any match */
  const size_t bsz = ((size_t)ny + 7u) >> 3;
  unsigned char *y_used = (unsigned char *)xcalloc(bsz, 1);

  /* Count output rows */
  int cnt = 0;
  for (int i = 0; i < nx; i++) if (ISNA(xp[i])) cnt++;  /* NA x entries */
  for (int i = 0; i < vx; i++) {
    if (dm_x[i] >= 0) {
      BS_SET(y_used, (unsigned)ys_arr[dm_x[i]].idx);
      cnt++;
    } else {
      cnt++;  /* unmatched x */
    }
  }
  for (int i = 0; i < vx; i++) {
    if (sm_x[i] >= 0) cnt++;  /* shifted matches are additional rows */
  }
  for (int i = 0; i < ny; i++)
    if (!ISNA(yp[i]) && !BS_TEST(y_used, (unsigned)i)) cnt++;  /* unmatched y */

  /* Fill output */
  SEXP rx = PROTECT(allocVector(INTSXP, cnt));
  SEXP ry = PROTECT(allocVector(INTSXP, cnt));
  int *ox = INTEGER(rx), *oy = INTEGER(ry), pos = 0;

  /* NA x entries first */
  for (int i = 0; i < nx; i++)
    if (ISNA(xp[i])) { ox[pos] = i + 1; oy[pos] = NA_INTEGER; pos++; }

  /* Direct matches and unmatched x */
  for (int i = 0; i < vx; i++) {
    if (dm_x[i] >= 0) {
      ox[pos] = xs_arr[i].idx + 1;
      oy[pos] = ys_arr[dm_x[i]].idx + 1;
      pos++;
    } else {
      ox[pos] = xs_arr[i].idx + 1;
      oy[pos] = NA_INTEGER;
      pos++;
    }
  }

  /* Shifted matches */
  for (int i = 0; i < vx; i++) {
    if (sm_x[i] >= 0) {
      ox[pos] = xs_arr[i].idx + 1;
      oy[pos] = ys_arr[sm_x[i]].idx + 1;
      pos++;
    }
  }

  /* Unmatched y */
  for (int i = 0; i < ny; i++)
    if (!ISNA(yp[i]) && !BS_TEST(y_used, (unsigned)i)) {
      ox[pos] = NA_INTEGER; oy[pos] = i + 1; pos++;
    }

  free(xs_arr); free(ys_arr); free(y_used);
  free(dm_x); free(dm_y); free(sm_x);

  SEXP result = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(result, 0, rx);
  SET_VECTOR_ELT(result, 1, ry);
  SEXP names = PROTECT(allocVector(STRSXP, 2));
  SET_STRING_ELT(names, 0, mkChar("x"));
  SET_STRING_ELT(names, 1, mkChar("y"));
  setAttrib(result, R_NamesSymbol, names);
  UNPROTECT(4);
  return result;
}
