#' All Filters (mean, median, kuan and lee) for Speckle Noise
#'
#' Applies all filters to a grayscale image or raster to reduce speckle noise.
#'
#' @param image A numeric matrix, raster::RasterLayer, or terra::SpatRaster representing the image.
#' @param window_size An integer specifying the size of the local window for median/mean filters. Default is 3.
#' @param ENL Effective Number of Looks. If NULL, it is estimated per filter or you can use the function estimae_enl()
#' @param plot_result Logical. If TRUE, plots Original + filtered images.
#' @return A named list with filtered images: $lee, $kuan, $mean, $median
#' @examples
#' \dontrun{
#' img <- load_example()
#' filters <- all_filters(img, window_size = 3, ENL = 3.5, plot_result = TRUE)
#' }
#' @export

all_filters <- function(image, window_size = 3, ENL = NULL, plot_result = TRUE) {

  # Check input
  if (is.null(image)) stop("Input image cannot be NULL")

  # Apply filters
  lee_res    <- lee_filter(image, window_size = window_size, ENL = ENL)
  kuan_res   <- kuan_filter(image, window_size = window_size, ENL = ENL)
  mean_res   <- mean_filter(image, window_size = window_size)
  median_res <- median_filter(image, window_size = window_size)

  # Collect results in a named list
  filters <- list(
    lee    = lee_res,
    kuan   = kuan_res,
    mean   = mean_res,
    median = median_res
  )

  # Plot results
  if (plot_result) {
    n <- length(filters) + 1  # +1 for Original
    rows <- 2
    cols <- ceiling(n / rows)
    par(mfrow = c(rows, cols), mar = c(2,2,2,2))

    # Plot Original
    if (inherits(image, "SpatRaster")) {
      terra::plot(log1p(image), main="Original", stretch="hist")
    } else if (inherits(image, "RasterLayer")) {
      raster::plot(log1p(image), main="Original", stretch="hist")
    } else if (is.matrix(image)) {
      image(log1p(image), main="Original", stretch="hist", col=gray.colors(256), axes=FALSE)
    }

    # Plot all filtered results
    for (name in names(filters)) {
      f <- filters[[name]]
      if (inherits(f, "SpatRaster")) {
        terra::plot(log1p(f), main=paste(name, "Filter"))
      } else if (inherits(f, "RasterLayer")) {
        raster::plot(log1p(f), main=paste(name, "Filter"))
      } else if (is.matrix(f)) {
        image(log1p(f), main=paste(name, "Filter"), col=gray.colors(256), axes=FALSE)
      }
    }
  }

  return(filters)
}
