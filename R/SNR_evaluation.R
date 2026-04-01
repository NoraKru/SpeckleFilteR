#' Signal to Noise Ratio (SNR) for Evaluation
#'
#' Calculate the Signal to Noise Ratio (SNR) to measure, which filter has the best outcome.
#'
#' Source: Comparison of Various Speckle Noise Reduction Filters on Synthetic Aperture Radar Image.Ardhi Wicaksono Santoso, Luhur Bayuaji, Lim Tien Sze, Habibah Lateh, Jasni Mohamad Zain
#' #'
#' @param image A numeric matrix, raster::RasterLayer, or terra::SpatRaster representing the image.
#' @param filtered_image filtered image
#' @param filter_name The name of the filter, which is used
#' @return Signal to Noise Ratio
#' @examples
#' img <- load_example()
#' filters <- all_filters(img, window_size = 3, ENL = 3.5, plot_result = FALSE)
#' snr_metrics <- SNR_evaluation(img, filters)
#' print(snr_metrics)
#' @export
#'

SNR_evaluation <- function(image, filtered_image, filter_name = NULL) {

  # Prepare original image and convert to matrix
  info <- .prepare_image(image)
  img <- info$matrix

  # Case 1: Multiple filtered images provided as a list
  if (is.list(filtered_image)) {

    results <- lapply(names(filtered_image), function(name) {
      f_img <- filtered_image[[name]]

      # Prepare the current filtered image
      filtered_info <- .prepare_image(f_img)
      img_filtered <- filtered_info$matrix

      # Flatten matrices to vectors for power calculation
      S <- as.vector(img)
      Sh <- as.vector(img_filtered)

      # Calculate Signal Power and Noise Power (residual difference)
      signal_power <- sum(S^2)
      noise_power <- sum((Sh - S)^2)

      # Calculate SNR in decibels (dB)
      snr <- 10 * log10(signal_power / noise_power)

      data.frame(
        filter = name,
        SNR = snr
      )
    })

    # Combine all results into a single data frame
    return(do.call(rbind, results))
  }

  # Case 2: Single filtered image provided
  filtered_info <- .prepare_image(filtered_image)
  img_filtered <- filtered_info$matrix

  # Flatten matrices to vectors
  S <- as.vector(img)
  Sh <- as.vector(img_filtered)

  # Calculate powers for the SNR formula
  signal_power <- sum(S^2)
  noise_power <- sum((Sh - S)^2)

  # Final SNR calculation in dB
  snr <- 10 * log10(signal_power / noise_power)

  # Return formatted result
  if (!is.null(filter_name)) {
    return(data.frame(filter = filter_name, SNR = snr))
  } else {
    return(snr)
  }
}
