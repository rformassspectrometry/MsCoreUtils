// Pascal Maas - p.maas@lacdr.leidenuniv.nl

#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

double weightedMean(NumericVector mzs, NumericVector ints) {
    // Get the total intensity
    double intensity = sum(ints);

    // Calculate the weighted mass
    int n = ints.length();
    double total = 0;
    for (int i = 0; i < n; i++) {
        total += mzs[i] * ints[i] / intensity;
    }
    return total;
}

double descend(int centroid, NumericVector intensities,
               NumericVector masses, double signalRatio) {

    // Maximum deviation from the centroid index
    int maxK = 30;

    // Define a min and max index for the region where the region is not negative
    // and does not exceed the size of the intensities vector, since we only
    // have to consider centroid - maxK until centroid + maxK

    // Ensures the index is at least 0
    int minId = std::max(centroid - maxK, 0);

    // Ensures the index is at most the size of the intensities vector - 1
    int maxId = std::min(centroid + maxK, (int) intensities.size() - 1);

    // Get indices of this region
    Rcpp::Range idx = Rcpp::Range(minId, maxId);

    // Subset intensities and masses with the defined region
    NumericVector ints = intensities[idx];
    NumericVector mass = masses[idx];


    // Find the new location of the centroid, as the centroid can be shifted by
    // subsetting the region. By substracting the minimum value, the original
    // centroid will now refer to the new centroid location.
    centroid = centroid - minId;
    double centroidValue = ints[centroid];

    // Calculate the allowed limits within the region
    // Update the left and right limits with new idx, as long as the values are
    // within the thresholds of percentage and are not rising.
    int leftLimit = centroid;

    // Start looping from centroid to the left (negative)
    for (int i = centroid - 1; i >= 0; i--) {
        // Get current and previous values
        double previous = ints[i + 1];
        double current = ints[i];

        // Good signal should be above the centroid / signal ratio
        bool goodSignal =  current / centroidValue > signalRatio;

        // Compare current to previous value to check for a rising signal
        bool risingSignal = current >= previous;

        // If either the signal is too low or the current signal is rising,
        // Break the loop since we've reached the left limit
        if (!goodSignal || risingSignal) {
            break;
        }
        // All good, so update the leftLimit to the current index
        leftLimit = i;
    }

    // Repeat this process for the right side of the centroid
    int rightLimit = centroid;
    int N = ints.size();

    // Start loop from centroid to right (positive)
    for (int i = centroid + 1; i < N; i++) {
        // Get current and previous values
        double current = ints[i];
        double previous = ints[i - 1];

        // Good signal should be above the centroid / signal ratio
        bool goodSignal =  current / centroidValue > signalRatio;

        // Compare current to previous value to check for a rising signal
        bool risingSignal = current >= previous;

        // If either the signal is too low or the current signal is rising,
        // Break the loop since we've reached the right limit
        if (!goodSignal || risingSignal) {
            break;
        }

        // All good, so update the rightLimit to the current index
        rightLimit = i;
    }

    // Determine the region between the left and right limits
    // to obtain the region of interest
    Range region = Rcpp::Range(leftLimit, rightLimit);

    // calculate the weighted mean of the region and return the value
    return weightedMean(mass[region], ints[region]);
}

//' @name descendPeaksC
//' @title descendPeak algorithm from MSnbase for centroid refinement
//' @description This is the C++ version of `descendPeak` of MSnbase. It
//' calculates the weighted mean of the centroid region by checking for the
//' signal ratio and rising signals.
//' @details DescendPeaks is a centroid-refinement algorithm that descends from
//' the centroid until the signal rises again. It also considers the intensity
//' of neighbouring signals to be at least a percentage of the centroid
//' intensity.
//' @param centroids Position of the centroids (local maximum) as C-index
//' (R-index - 1).
//' @param intensities Vector of intensities in a scan, preferably smoothed
//' @param masses Vector of masses in a scan
//' @param signalPercentage Percentage between 0-100 defining the minimum signal
//' for a peak to be considered for the weighted intensity region.
//' @return Weighted masses for the given centroids
//' @export
// [[Rcpp::export]]
NumericVector descendPeaksC(NumericVector centroids, NumericVector intensities,
                           NumericVector masses, double signalPercentage) {

    // Prepare variables
    int n = centroids.length();
    double signalRatio = signalPercentage / 100;
    NumericVector weightedMZs(n);

    // Loop through all centroid positions (should be C-indexed, not R-indexed!)
    for (int i = 0; i < n; i++) {
        // Calculate the weighted mass
        weightedMZs[i] = descend(centroids[i], intensities,
                                 masses, signalRatio);
    }
    return weightedMZs;
}
