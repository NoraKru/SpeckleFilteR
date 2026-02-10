#' Load Example Sentinel Image
#'
#' Loads the example Sentinel-1 TIF included in the package.
#'
#' @return A SpatRaster object
#' @examples
#' img <- load_example()
#' plot(img)
#' @export
load_example <- function() {
  f <- system.file("extdata", "sentinel.tif", package = "SpeckleFilteR")
  if (f == "") stop("Example file not found in the package.")
  terra::rast(f)
}
