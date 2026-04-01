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
#' enl <- estimate_ENL(img)  # interactive: select region with mouse
#' }
#' @export
#' @importFrom terra rast plot draw crop values crs ext
estimate_ENL <- function(raster) {

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
  #terra::plot(log10(rast_obj + 1e-6), main="Please select a homogeneous area in the satellite image.\n Click multiple points to define a polygon.\n Press ESC when done. ")
  # 1. Vorbereitung (wie gehabt)
  all_values <- as.matrix(rast_obj)
  color_breaks <- quantile(all_values, probs = seq(0, 1, length.out = 257), na.rm = TRUE)
  color_breaks <- unique(color_breaks)
  my_cols <- viridis::viridis(length(color_breaks) - 1)
  layout(matrix(c(1, 2), nrow = 1), widths = c(1, 4))
  # 2. Plotten mit terra::plot
  # Wir deaktivieren die Standard-Legende (plegend=FALSE),
  # um die volle Kontrolle zu behalten, ODER wir konfigurieren sie:

  # --- 1. Zuerst die Legende (geht in den linken Slot) ---
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1))

  # Balken etwas weiter links positionieren (0.1 bis 0.3)
  rasterImage(as.raster(rev(viridis::viridis(256))), 0.1, 0.05, 0.3, 0.95)

  break_indices <- round(seq(1, length(color_breaks), length.out = 5))
  tick_labels    <- round(color_breaks[break_indices], 3)
  tick_positions <- seq(0.05, 0.95, length.out = 5)

  # Achse diesmal auf der LINKEN Seite des Balkens (axis side 2) oder RECHTS (side 4)
  # Ich lasse sie rechts vom Balken (side 4), aber innerhalb des linken Slots
  axis(4, at = tick_positions, labels = FALSE, pos = 0.35, tcl = -0.2)
  text(x = 0.4, y = tick_positions, labels = tick_labels, adj = 0, xpd = TRUE, cex = 0.9)
  mtext("Scala", side = 3, at = 0.2, line = 0.5, font = 2)

  # --- 2. Dann das Bild (geht in den rechten Slot) ---
  # WICHTIG: Da dies der LETZTE Plot ist, bleibt das Koordinatensystem
  # für terra::draw() hier aktiv!
  terra::plot(rast_obj,
              main = "Select area (Click points, then ESC)",
              col = my_cols,
              breaks = color_breaks,
              axes = TRUE,
              mar = c(3, 1, 3, 3), # Margins anpassen
              legend = FALSE)
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
