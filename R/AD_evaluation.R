#' Average Difference (AD) for Evaluation the different filters
#'
#' Calculate the Average Difference to measure, which filter has the best outcome.
#'
#' Source: Comparison of Various Speckle Noise Reduction Filters on Synthetic Aperture Radar Image.Ardhi Wicaksono Santoso, Luhur Bayuaji, Lim Tien Sze, Habibah Lateh, Jasni Mohamad Zain
#'
#' @param image A numeric matrix, raster::RasterLayer, or terra::SpatRaster representing the image.
#' @param filtered_image filtered image
#' @param filter_name The name of the filter, which is used
#' @return Average Difference
#' @examples
#' img <- load_example()
#' filters <- all_filters(img, window_size = 3, ENL = 3.5, plot_result = FALSE)
#' ad_metrics <- AD_evaluation(img, filters)
#' print(ad_metrics)
#' @export
#'

AD_evaluation <- function(image, filtered_image, filter_name = NULL) {

  # Prepare original image and extract dimensions
  info <- .prepare_image(image)
  img <- info$matrix
  n_r <- nrow(img)
  n_c <- ncol(img)

  #Case 1: Multiple filtered images provided as a list
  if (is.list(filtered_image)) {

    results <- lapply(names(filtered_image), function(name) {
      f_img <- filtered_image[[name]]

      # Prepare the current filtered image
      filtered_info <- .prepare_image(f_img)
      img_filtered <- filtered_info$matrix

      #Initialize sum for Absolute Difference (AD)
      ad_sum <- 0
      # Perform pixel-wise absolute difference calculation
      for (i in 1:n_r) {
        for (j in 1:n_c) {
          ad_sum <- ad_sum + abs(img[i, j] - img_filtered[i, j])
        }
      }

      # Calculate mean absolute difference across all pixels
      ad <- ad_sum / (n_r * n_c)

      data.frame(
        filter = name,
        AD = ad
      )
    })

    # Combine all results into a single data frame
    return(do.call(rbind, results))
  }

  # Case 2: Single filtered image provided
  filtered_info <- .prepare_image(filtered_image)
  img_filtered <- filtered_info$matrix

  ad_sum <-0

  # Iterate over rows and columns for pixel-wise comparison
  for (i in 1:n_r) {       # iterate over rows
    for (j in 1:n_c) {     # iterate over columns
      ad_sum <- ad_sum + abs(img[i,j]-img_filtered[i,j])
    }
  }

  # Final AD calculation
  ad <-ad_sum/(n_r*n_c)

  # Return result as data.frame if a filter name is provided, otherwise as a numeric value
  if (!is.null(filter_name)) {
    return(data.frame(filter = filter_name, AD = ad))
  } else {
    return(ad)
  }
}

