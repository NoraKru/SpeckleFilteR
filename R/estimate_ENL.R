#' Estimate ENL interactively from a satellite raster
#'
#' Allows the user to select a homogeneous, flat area in a satellite image
#' to estimate the Equivalent Number of Looks (ENL) for speckle filtering.
#' Click multiple points to define a polygon and press ESC when done.
#'
#' @param raster A numeric matrix, raster::RasterLayer or terra::SpatRaster.
#'               Only the first band will be used for interactive selection. Matrices and RasterLayers
#'               will automatically be converted to a \code{SpatRaster}.
#' @return A numeric value representing the estimated ENL.
#' @examples
#' \dontrun{
#' img <- load_example()
#' enl <- estimate_enl(img)  # interactive: select region with mouse
#' }
#' @export
estimate_enl <- function(raster) {

  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' is required for this function.")
  }

  # --- Convert input to SpatRaster if necessary ---
  if (inherits(raster, "SpatRaster")) {
    rast_obj <- raster
  } else if (inherits(raster, "RasterLayer")) {
    rast_obj <- terra::rast(raster)
  } else if (is.matrix(raster)) {
    rast_obj <- terra::rast(raster)
    # Set default extent so coordinates exist
    terra::ext(rast_obj) <- c(0, ncol(raster), 0, nrow(raster))
  } else {
    stop("Input must be a numeric matrix, RasterLayer, or SpatRaster")
  }

  # --- Instructions for the user ---
  cat("****************************************\n")
  cat("ENL not provided.\n")
  cat("Please select a homogeneous, flat area in the satellite image.\n")
  cat("Click multiple points to define a polygon.\n")
  cat("Press ESC when done.\n")
  cat("****************************************\n")

  dev.new(noRStudioGD = TRUE)

  # --- Plot first band for interactive selection ---
  terra::plot(log10(rast_obj + 1e-6), main="Select AOI (log10)")

  # --- Draw polygon ---
  aoi <- terra::draw(x="polygon")


  # --- Ensure CRS matches ---
  if (is.na(terra::crs(aoi))) terra::crs(aoi) <- terra::crs(rast_obj)

  rast_aoi <- terra::crop(rast_obj, aoi)


  # --- Extract values as numeric vector ---
  vals <- terra::values(rast_aoi, mat = FALSE)
  vals <- as.numeric(vals)
  vals <- vals[!is.na(vals)]

  # --- Calculate ENL ---
  ENL <- mean(vals)^2 / var(vals)

  message(sprintf("Estimated ENL from selected AOI: %.2f", ENL))
  return(ENL)
}


