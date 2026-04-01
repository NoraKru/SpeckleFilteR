#' Mean Square Error for Evaluation the different Filter
#'
#' Calculates the Mean Square Error to measure, which filter has the best outcome.
#'
#' Source: Comparison of Various Speckle Noise Reduction Filters on Synthetic Aperture Radar Image.Ardhi Wicaksono Santoso, Luhur Bayuaji, Lim Tien Sze, Habibah Lateh, Jasni Mohamad Zain
#' #'
#' @param image A numeric matrix, raster::RasterLayer, or terra::SpatRaster representing the image.
#' @param filtered_image filtered image
#' @param filter_name The name of the filter, which is used
#' @return Mean Square Error
#' @examples
#' img <- load_example()
#' filters <- all_filters(img, window_size = 3, ENL = 3.5, plot_result = FALSE)
#' mse_metrics <- MSE_evaluation(img, filters)
#' print(mse_metrics)
#' @importFrom stats quantile mean sd var median rank
#' @export
#'

MSE_evaluation <- function(image, filtered_image, filter_name = NULL) {

  # Prepare original image and extract dimensions
  info <- .prepare_image(image)
  img <- info$matrix
  n_r <- nrow(img)
  n_c <- ncol(img)

  # Case 1: Multiple filtered images provided as a list
  if (is.list(filtered_image)) {

    results <- lapply(names(filtered_image), function(name) {
      f_img <- filtered_image[[name]]

      # Prepare the current filtered image
      filtered_info <- .prepare_image(f_img)
      img_filtered <- filtered_info$matrix

      # Initialize sum for Mean Squared Error (MSE)
      mse_sum <- 0
      # Perform pixel-wise squared difference calculation
      for (i in 1:n_r) {
        for (j in 1:n_c) {
          mse_sum <- mse_sum + (img[i,j] - img_filtered[i,j])^2
        }
      }
      # Calculate mean squared difference across all pixels
      mse <- mse_sum / (n_r * n_c)

      data.frame(
        filter = name,
        MSE = mse
      )
    })

    # Combine all results into a single data frame
    return(do.call(rbind, results))
  }

  # Case 2: Single filtered image provided
  filtered_info <- .prepare_image(filtered_image)
  img_filtered <- filtered_info$matrix

  mse_sum <- 0

  # Iterate over rows and columns for pixel-wise comparison
  for (i in 1:n_r) {
    for (j in 1:n_c) {
      mse_sum <- mse_sum + (img[i,j] - img_filtered[i,j])^2
    }
  }
  # Final MSE calculation
  mse <- mse_sum / (n_r * n_c)

  # Return result as data.frame if a filter name is provided, otherwise as a numeric value
  if (!is.null(filter_name)) {
    return(data.frame(filter = filter_name, MSE = mse))
  } else {
    return(mse)
  }
}
