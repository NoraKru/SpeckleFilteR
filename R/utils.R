# Internal helper:
# Prepares input image for filtering by converting raster inputs
# to matrices and storing metadata required for reconstruction.

.prepare_image <- function(image) {
  if (inherits(image, "SpatRaster")) {
    list(
      matrix = terra::as.matrix(image, wide = TRUE),
      is_raster = TRUE,
      raster_type = "terra",
      ref = image
    )
  } else if (inherits(image, "RasterLayer")) {
    list(
      matrix = as.matrix(image, wide = TRUE),
      is_raster = TRUE,
      raster_type = "raster",
      ref = image
    )
  } else if (is.matrix(image)) {
    list(
      matrix = image,
      is_raster = FALSE,
      raster_type = NULL,
      ref = NULL
    )
  } else {
    stop("Input must be a matrix or raster")
  }
}

# Internal helper:
# Reconstructs output image to original raster type if required.

.reconstruct_image <- function(filtered_matrix, info) {

  if (!info$is_raster) {
    return(filtered_matrix)
  }

  if (info$raster_type == "terra") {
    r <- terra::rast(filtered_matrix)
    terra::ext(r) <- terra::ext(info$ref)
    terra::crs(r) <- terra::crs(info$ref)
    return(r)
  }

  if (info$raster_type == "raster") {
    r <- raster::raster(filtered_matrix)
    raster::extent(r) <- raster::extent(info$ref)
    raster::crs(r) <- raster::crs(info$ref)
    return(r)
  }
}
