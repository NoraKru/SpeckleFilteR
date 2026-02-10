#' Mean Filter for Speckle Noise
#'
#' Applies a mean filter to a grayscale image or raster to reduce speckle noise.
#'
#' @param image A numeric matrix, raster::RasterLayer, or terra::SpatRaster representing the image.
#' @param window_size An integer specifying the size of the mean filter window. Default is 3.
#' @return The filtered image. Returns a matrix if input was a matrix, RasterLayer if input was raster::RasterLayer, or SpatRaster if input was terra::SpatRaster.
#' @examples
#' \dontrun{
#' img <- load_example()
#' mean_f <- mean_filter(img, window_size = 3)
#' }
#' @export
#'


mean_filter <- function(image, window_size = 3) {

  info <- .prepare_image(image)
  img <- info$matrix

  nrow <- nrow(img)
  ncol <- ncol(img)
  radius <- floor(window_size / 2)

  filtered <- matrix(0, nrow = nrow, ncol = ncol)

  for (i in 1:nrow) {
    for (j in 1:ncol) {
      row_range <- max(1, i - radius) : min(nrow, i + radius)
      col_range <- max(1, j - radius) : min(ncol, j + radius)
      window <- img[row_range, col_range]
      filtered[i, j] <- mean(window)
    }
  }

  # filtered_matrix <- as.matrix(filtered)
  result <- .reconstruct_image(filtered, info)

  return(result)
}
