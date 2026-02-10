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

  # Check input type for image and convert to matrix if needed
  info <- .prepare_image(image)
  img <- info$matrix

  # Check input type for filtered_image and convert to matrix if needed
  filtered_info <- .prepare_image(filtered_image)
  img_filtered <- filtered_info$matrix

  n_r <- nrow(img)
  n_c <- ncol(img)
  mse_sum <-0

  for (i in 1:n_r) {       # iterate over rows
    for (j in 1:n_c) {     # iterate over columns
      mse_sum <- mse_sum + (img[i,j]-img_filtered[i,j])^2
    }
  }
  mse <-mse_sum/(n_r*n_c)

  max_I <- max(img)
  psnr <- 10*log10((max_I/mse))

  if (!is.null(filter_name)) {
    return(data.frame(filter = filter_name, PSNR = psnr))
  } else {
    return(psnr)
  }
}
