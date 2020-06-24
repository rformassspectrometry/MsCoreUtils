# Low level functions for MS data

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![build status](https://travis-ci.org/rformassspectrometry/MsCoreUtils.svg?branch=master)](https://travis-ci.org/rformassspectrometry/MsCoreUtils)
[![codecov.io](https://codecov.io/github/rformassspectrometry/MsCoreUtils/coverage.svg?branch=master)](https://codecov.io/github/rformassspectrometry/MsCoreUtils?branch=master)
[![license](https://img.shields.io/badge/license-Artistic--2.0-brightgreen.svg)](https://opensource.org/licenses/Artistic-2.0)

<img
src="https://raw.githubusercontent.com/rformassspectrometry/stickers/master/MsCoreUtils/MsCoreUtils.png"
height="150">

This package aims to be a common place for useful mass spectrometry functions
that are needed in multiple packages. We hope to keep the dependencies as small
as possible.

Currently we externalising core functions from `MSnbase`, `MALDIquant`, etc.

# Contributions

- Sigurdur Smarason (@SiggiSmara): weighted moving average (https://github.com/sgibb/MALDIquant/pull/54)
- Thomas Naake (@tnaake): dotproduct calculation (https://github.com/rformassspectrometry/MsCoreUtils/pull/17)
- Adriaan Sticker: `robustSummary` aggregation function (originally contributed to `MSnbase`)
