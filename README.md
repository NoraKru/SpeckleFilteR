# SpeckleFilteR

`SpeckleFilteR` is an R package for applying and evaluating speckle filters on SAR images (e.g., Sentinel-1) that have been **preprocessed with orbit correction and calibration**. It provides several popular filters and metrics to help you select the best filter for your data.

Filters: Median Filter, Mean Filter, Lee Filter and Kuan Filter 

Metrics: Mean Square Error (MSE), Average Difference (AD), Speckle Index (SI), Peak Signal to Noise Ratio (PSNR) and Signal to Noise Ratio (SNR)

## Installation
The package was developed on R version 4.5.1.

You can install the package directly from GitHub:

```r
# install.packages("devtools") if not already installed
remotes::install_github("NoraKru/SpeckleFilteR")
```

## Introduction

Speckle is a granular noise that naturally occurs in coherent imaging systems, such as synthetic aperture radar (SAR), degrading image quality. It results from the interference of multiple scattered waves and can be reduced using specialized speckle filters while trying to preserve important image details.

`SpeckleFilteR` offers a streamlined workflow to:

**1.** Apply different speckle filters (Kuan, Lee, Median, Mean)

**2.** Evaluate filter performance using multiple metrics (MSE, AD, SI, PSNR, SNR)

**3.** Identify the best performing filter

This package is useful for researchers and practitioners working with noisy data or imagery

## Example

```r
# Note: Make sure the SAR image has been preprocessed with orbit correction and calibration

library(SpeckleFilteR)

# Load the noisy example image
sentinel1 <- load_example()

# Estimate the Equivalent Number of Looks (ENL) if not known
enl <- estimate_ENL(sentinel1)

# Apply all speckle filters (Kuan, Lee, Median, Mean)
results <- all_filters(sentinel1, ENL=enl)

# Evaluate filter performance using multiple metrics
evaluation <- evaluate_all_metrics(sentinel1, results)

#Table with all filters and their metric results
evaluation$metrics

#Summary sentence indicating which filter performed best
evaluation$summary
```


