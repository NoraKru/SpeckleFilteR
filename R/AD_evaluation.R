#' Average Difference (AD) for Evaluation the different filters
#'
#' Calculate the Average Difference to measure, which filter has the best outcome.
#'
#' Source: Comparison of Various Speckle Noise Reduction Filters on Synthetic Aperture Radar Image.Ardhi Wicaksono Santoso, Luhur Bayuaji, Lim Tien Sze, Habibah Lateh, Jasni Mohamad Zain
#' #'
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

  # Check input type for image and convert to matrix if needed
  info <- .prepare_image(image)
  img <- info$matrix

  n_r <- nrow(img)
  n_c <- ncol(img)

  if (is.list(filtered_image)) {

    results <- lapply(names(filtered_image), function(name) {
      f_img <- filtered_image[[name]]

      filtered_info <- .prepare_image(f_img)
      img_filtered <- filtered_info$matrix

      ad_sum <- 0
      for (i in 1:n_r) {
        for (j in 1:n_c) {
          ad_sum <- ad_sum + abs(img[i, j] - img_filtered[i, j])
        }
      }

      ad <- ad_sum / (n_r * n_c)

      data.frame(
        filter = name,
        AD = ad
      )
    })

    return(do.call(rbind, results))
  }

  # Check input type for filtered_image and convert to matrix if needed
  filtered_info <- .prepare_image(filtered_image)
  img_filtered <- filtered_info$matrix

  ad_sum <-0

  for (i in 1:n_r) {       # iterate over rows
    for (j in 1:n_c) {     # iterate over columns
      ad_sum <- ad_sum + abs(img[i,j]-img_filtered[i,j])
    }
  }
  ad <-ad_sum/(n_r*n_c)

  if (!is.null(filter_name)) {
    return(data.frame(filter = filter_name, AD = ad))
  } else {
    return(ad)
  }
}

