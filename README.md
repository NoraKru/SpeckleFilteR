# SpeckleFilteR

`SpeckleFilteR` is an R package for applying and evaluating speckle filters on SAR images (e.g., Sentinel-1) that have been **preprocessed with orbit correction and calibration**. It provides several popular filters and metrics to help you select the best filter for your data.

Filters: Median Filter, Mean Filter, Lee Filter and Kuan Filter 

Metrics: Mean Square Error (MSE), Average Difference (AD), Speckle Index (SI), Peak Signal to Noise Ratio (PSNR) and Signal to Noise Ratio (SNR)

## Installation
The package was developed on R version 4.5.1.

You can install the package directly from GitHub:

```r
# List of required packages
packages <- c("terra", "raster", "remotes")

# Check which packages are missing
missing_packages <- packages[!(packages %in% installed.packages()[,"Package"])]

# Install only missing packages
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
} else {
  message("All packages are already installed!")
}
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
## Mathematical Background & Methodology

**Speckle Filter**

Mean Filter: A simple linear filter that replaces each pixel value with the average of its neighbors in an N×N window.
<img width="1746" height="531" alt="mean_filter" src="https://github.com/user-attachments/assets/ffc4759a-233a-4293-91e0-5d6d59a9d0e0" />

Median Filter: A non-linear filter that replaces the center pixel with the median value of the NxN window.
<img width="2350" height="958" alt="MedianFilter" src="https://github.com/user-attachments/assets/c33d0421-0849-462a-8613-7f645733bae5" />

Lee Filter: The Lee filter is an adaptive speckle reduction tool that balances noise smoothing with edge preservation. It calculates a weighting factor W by comparing known sensor noise (Cu) to the local variance (Ci) of each window. In high-contrast areas like edges, the filter maintains the original pixel values to keep the image sharp. In homogeneous regions, it applies strong smoothing (similar to a mean filter) to effectively remove graininess without blurring structural details.
<img width="2349" height="845" alt="RplotLeeFilter" src="https://github.com/user-attachments/assets/ee299d45-ef64-4077-b0cf-1b6ec8c2c0bd" />


## Dependencies

- R >= 4.0
- Required packages: terra, raster

## References

- Lecture notes: Tobias Ullmann, Julius-Maximilians-Universität Würzburg
- Santoso, A.W., Bayuaji, L., Lim, T.S., Lateh, H., & Zain, J.M. (2016). *Comparison of Various Speckle Noise Reduction Filters on Synthetic Aperture Radar Image.* International Journal of Applied Engineering Research, 11(15), 8760–8767. [http://www.ripublication.com](http://www.ripublication.com)



