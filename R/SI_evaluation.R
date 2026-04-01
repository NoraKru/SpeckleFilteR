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
#' @importFrom stats quantile mean sd var median rank
#' @export
#'

SI_evaluation <- function(image, filtered_image, radius = 1, filter_name = NULL) {

  # Prepare original image and extract dimensions
  info <- .prepare_image(image)
  img <- info$matrix
  n_r <- nrow(img)
  n_c <- ncol(img)

  # Case 1: Multiple filtered images provided as a list (e.g., output from all_filters)
  if (is.list(filtered_image)) {

    results <- lapply(names(filtered_image), function(name) {
      f_img <- filtered_image[[name]]
      # Prepare the current filtered image
      filtered_info <- .prepare_image(f_img)
      img_filtered <- filtered_info$matrix

      si_sum <- 0

      # Perform pixel-wise local window calculation
      for (i in 1:n_r) {
        for (j in 1:n_c) {

          # Define local window boundaries based on radius
          row_range <- max(1, i - radius):min(n_r, i + radius)
          col_range <- max(1, j - radius):min(n_c, j + radius)
          window <- img_filtered[row_range, col_range]

          # Calculate local mean (mu) and standard deviation (sigma)
          mu <- mean(window)
          sigma <- sd(as.vector(window))

          # Add Coefficient of Variation (sigma/mu) to the sum
          if (mu != 0) {
            si_sum <- si_sum + (sigma / mu)
          }
        }
      }

      # Calculate average Speckle Index across the image
      si <- si_sum / (n_r * n_c)

      data.frame(
        filter = name,
        SI = si
      )
    })

    # Combine all results into a single data frame
    return(do.call(rbind, results))
  }

  # Case 2: Single filtered image provided
  filtered_info <- .prepare_image(filtered_image)
  img_filtered <- filtered_info$matrix

  si_sum <- 0

  # Iterate over rows and columns for local window comparison
  for (i in 1:n_r) {
    for (j in 1:n_c) {

      # Define local window boundaries
      row_range <- max(1, i - radius):min(n_r, i + radius)
      col_range <- max(1, j - radius):min(n_c, j + radius)
      window <- img_filtered[row_range, col_range]

      # Calculate local statistics
      mu <- mean(window)
      sigma <- sd(as.vector(window))

      # Sum the local SI components
      if (mu != 0) {
        si_sum <- si_sum + (sigma / mu)
      }
    }
  }

  # Final SI calculation
  si <- si_sum / (n_r * n_c)

  # Return formatted result
  if (!is.null(filter_name)) {
    return(data.frame(filter = filter_name, SI = si))
  } else {
    return(si)
  }
}
