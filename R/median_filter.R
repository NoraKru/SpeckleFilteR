#' Median Filter for Speckle Noise
#'
#' Applies a median filter to a grayscale image or raster to reduce speckle noise.
#'
#' @param image A numeric matrix, raster::RasterLayer, or terra::SpatRaster representing the image.
#' @param window_size An integer specifying the size of the median filter window. Default is 3.
#' @return The filtered image. Returns a matrix if input was a matrix, RasterLayer if input was raster::RasterLayer, or SpatRaster if input was terra::SpatRaster.
#' @examples
#' \dontrun{
#' img <- load_example()
#' median_f <- median_filter(img, window_size = 3)
#' }
#' @export
#'


median_filter <- function(image, window_size = 3) {

  info <- .prepare_image(image)
  img  <- info$matrix

  n_r <- nrow(img)
  n_c <- ncol(img)
  radius <- floor(window_size / 2)

  filtered <- matrix(0, nrow = n_r, ncol = n_c)

  for (i in 1:n_r) {
    for (j in 1:n_c) {
      row_range <- max(1, i - radius) : min(n_r, i + radius)
      col_range <- max(1, j - radius) : min(n_c, j + radius)

      window <- img[row_range, col_range]
      filtered[i, j] <- median(window)
    }
  }

  # Return result in same type as input
  result <- .reconstruct_image(filtered, info)
  return(result)
}
