#' Peak Signal to Noise Ratio (PSNR) for Evaluation the different Filter
#'
#' Calculate the PSNR to measure, which filter has the best outcome.
#'
#' Source: Comparison of Various Speckle Noise Reduction Filters on Synthetic Aperture Radar Image.Ardhi Wicaksono Santoso, Luhur Bayuaji, Lim Tien Sze, Habibah Lateh, Jasni Mohamad Zain
#'
#' @param image A numeric matrix, raster::RasterLayer, or terra::SpatRaster representing the image.
#' @param filtered_image filtered image
#' @param filter_name The name of the filter, which is used
#' @return Peak Signal to Noise Ratio
#' @examples
#' img <- load_example()
#' filters <- all_filters(img, window_size = 3, ENL = 3.5, plot_result = FALSE)
#' psnr_metrics <- PSNR_evaluation(img, filters)
#' print(psnr_metrics)
#' @export
#'

PSNR_evaluation <- function(image, filtered_image, filter_name = NULL) {

  # Prepare original image and extract dimensions
  info <- .prepare_image(image)
  img <- info$matrix
  n_r <- nrow(img)
  n_c <- ncol(img)

  # Prepare filtered image and convert to matrix
  filtered_info <- .prepare_image(filtered_image)
  img_filtered <- filtered_info$matrix

  # Initialize sum for Mean Squared Error (MSE) calculation
  mse_sum <-0

  # Iterate over rows and columns for pixel-wise comparison
  for (i in 1:n_r) {
    for (j in 1:n_c) {
      mse_sum <- mse_sum + (img[i,j]-img_filtered[i,j])^2
    }
  }

  # Calculate Mean Squared Error (MSE)
  mse <-mse_sum/(n_r*n_c)

  # Calculate the Peak Signal-to-Noise Ratio (PSNR)
  # Uses the maximum intensity value of the original image
  max_I <- max(img)
  psnr <- 10*log10((max_I/mse))

  # Return result as data.frame if a filter name is provided, otherwise as a numeric value
  if (!is.null(filter_name)) {
    return(data.frame(filter = filter_name, PSNR = psnr))
  } else {
    return(psnr)
  }
}
