#' Speckle Index (S)) for Evaluation the different filters
#'
#' Calculate the Speckle Index to measure, which filter has the best outcome.
#'
#' Source: Comparison of Various Speckle Noise Reduction Filters on Synthetic Aperture Radar Image.Ardhi Wicaksono Santoso, Luhur Bayuaji, Lim Tien Sze, Habibah Lateh, Jasni Mohamad Zain
#' #'
#' @param image A numeric matrix, raster::RasterLayer, or terra::SpatRaster representing the image.
#' @param filtered_image filtered image
#' @param filter_name The name of the filter, which is used
#' @return Speckle Index
#' @examples
#' img <- load_example()
#' filters <- all_filters(img, window_size = 3, ENL = 3.5, plot_result = FALSE)
#' si_metrics <- SI_evaluation(img, filters)
#' print(si_metrics)
#' @export
#'

SI_evaluation <- function(image, filtered_image, radius = 1, filter_name = NULL) {

  info <- .prepare_image(image)
  img <- info$matrix
  n_r <- nrow(img)
  n_c <- ncol(img)

  # if it is the list (output from all:filters)
  if (is.list(filtered_image)) {

    results <- lapply(names(filtered_image), function(name) {

      f_img <- filtered_image[[name]]
      filtered_info <- .prepare_image(f_img)
      img_filtered <- filtered_info$matrix

      si_sum <- 0

      for (i in 1:n_r) {
        for (j in 1:n_c) {

          row_range <- max(1, i - radius):min(n_r, i + radius)
          col_range <- max(1, j - radius):min(n_c, j + radius)
          window <- img_filtered[row_range, col_range]

          mu <- mean(window)
          sigma <- sd(as.vector(window))

          if (mu != 0) {
            si_sum <- si_sum + (sigma / mu)
          }
        }
      }

      si <- si_sum / (n_r * n_c)

      data.frame(
        filter = name,
        SI = si
      )
    })

    return(do.call(rbind, results))
  }

  # only one filter
  filtered_info <- .prepare_image(filtered_image)
  img_filtered <- filtered_info$matrix

  si_sum <- 0

  for (i in 1:n_r) {
    for (j in 1:n_c) {

      row_range <- max(1, i - radius):min(n_r, i + radius)
      col_range <- max(1, j - radius):min(n_c, j + radius)
      window <- img_filtered[row_range, col_range]

      mu <- mean(window)
      sigma <- sd(as.vector(window))

      if (mu != 0) {
        si_sum <- si_sum + (sigma / mu)
      }
    }
  }

  si <- si_sum / (n_r * n_c)

  if (!is.null(filter_name)) {
    return(data.frame(filter = filter_name, SI = si))
  } else {
    return(si)
  }
}
