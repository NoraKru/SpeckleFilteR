#' Mean Square Error for Evaluation the different Filter
#'
#' Calculate the Mean Square Error to measure, which filter has the best outcome.
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
#' @export
#'

MSE_evaluation <- function(image, filtered_image, filter_name = NULL) {

  # Originalbild vorbereiten
  info <- .prepare_image(image)
  img <- info$matrix
  n_r <- nrow(img)
  n_c <- ncol(img)

  # ---- FALL 1: Liste von Filtern ----
  if (is.list(filtered_image)) {

    results <- lapply(names(filtered_image), function(name) {

      f_img <- filtered_image[[name]]
      filtered_info <- .prepare_image(f_img)
      img_filtered <- filtered_info$matrix

      mse_sum <- 0
      for (i in 1:n_r) {
        for (j in 1:n_c) {
          mse_sum <- mse_sum + (img[i,j] - img_filtered[i,j])^2
        }
      }
      mse <- mse_sum / (n_r * n_c)

      data.frame(
        filter = name,
        MSE = mse
      )
    })

    return(do.call(rbind, results))
  }

  # ---- FALL 2: Einzelner Filter ----
  filtered_info <- .prepare_image(filtered_image)
  img_filtered <- filtered_info$matrix

  mse_sum <- 0
  for (i in 1:n_r) {
    for (j in 1:n_c) {
      mse_sum <- mse_sum + (img[i,j] - img_filtered[i,j])^2
    }
  }
  mse <- mse_sum / (n_r * n_c)

  if (!is.null(filter_name)) {
    return(data.frame(filter = filter_name, MSE = mse))
  } else {
    return(mse)
  }
}
