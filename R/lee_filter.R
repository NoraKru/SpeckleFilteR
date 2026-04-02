#' Lee Filter for Speckle Noise
#'
#' Applies a Lee filter to a grayscale image/raster to reduce speckle noise.
#'
#' Source: Active Remote Sensing - Tobi Ullmann
#'
#' @param image A numeric matrix, raster::RasterLayer, or terra::SpatRaster representing the image.
#' @param window_size An integer specifying the size of the local window. Default is 3.
#' @param ENL Equivalent number of looks.
#' @return The filtered image. Returns a matrix if input was a matrix, RasterLayer if input was raster::RasterLayer, or SpatRaster if input was terra::SpatRaster.
#' @examples
#' \dontrun{
#' img <- load_example()
#' lee <- lee_filter(img, window_size = 3, ENL = 3.5)
#' }
#' @export
#'
lee_filter <- function(image, window_size = 3, ENL = NULL) {

  #with prepare image a raster or spatraster get transformed in to matrix
  info <- .prepare_image(image)
  img <- info$matrix

  # stop if ENL is not given
  if (is.null(ENL)) {
    stop("ENL not provided. Use estimate_ENL() or provide ENL manually.")
  }

  n_r <- nrow(img)
  n_c <- ncol(img)
  radius <- floor(window_size / 2)
  filtered <- matrix(0, nrow = n_r, ncol = n_c)

  # noise variation coefficient
  C_u <- sqrt(1 / ENL)

  #for Loop for calculating filtered image (see the mathematical background in the ReadMe)
  for (i in 1:n_r) {
    for (j in 1:n_c) {
      # define neighborhood window
      row_range <- max(1, i - radius) : min(n_r, i + radius)
      col_range <- max(1, j - radius) : min(n_c, j + radius)
      window <- img[row_range, col_range]

      # local mean and sd
      local_mean  <- mean(window)
      local_sigma <- sd(as.vector(window))

      if (local_mean == 0) {
        filtered[i, j] <- img[i, j]
        next
      }

      # Image variation coefficient
      C_i <- local_sigma / local_mean

      # weight
      W <- 1 - (C_u^2 / C_i^2)
      W <- max(0, min(W, 1))

      filtered[i, j] <- img[i, j] * W + local_mean * (1 - W)
    }
  }

  # Return result matrix in same type as input
  .reconstruct_image(filtered, info)
}
