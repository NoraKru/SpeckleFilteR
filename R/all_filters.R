#' All Filters (mean, median, kuan and lee) for Speckle Noise
#'
#' Applies all filters to a grayscale image or raster to reduce speckle noise.
#'
#' @param image A numeric matrix, raster::RasterLayer, or terra::SpatRaster representing the image.
#' @param window_size An integer specifying the size of the local window for median/mean filters. Default is 3.
#' @param ENL Effective Number of Looks. If NULL, it is estimated per filter or you can use the function estimate_enl()
#' @param plot_result Logical. If TRUE, plots Original + filtered images with optimized contrast.
#' @return A named list with filtered images: $lee, $kuan, $mean, $median
#' @export

#' All Filters (mean, median, kuan and lee) for Speckle Noise
#' @export

all_filters <- function(image, window_size = 3, ENL = NULL, plot_result = TRUE) {

  # 1. Filter anwenden
  lee_res    <- lee_filter(image, window_size = window_size, ENL = ENL)
  kuan_res   <- kuan_filter(image, window_size = window_size, ENL = ENL)
  mean_res   <- mean_filter(image, window_size = window_size)
  median_res <- median_filter(image, window_size = window_size)

  filters <- list(
    lee    = lee_res,
    kuan   = kuan_res,
    mean   = mean_res,
    median = median_res
  )

  if (plot_result) {
    # --- NEU: GEMEINSAME SKALA OHNE AUSREISSER ---

    # Alle Werte in einen Vektor kombinieren (Log-transformiert, wie in deinen Plots)
    # Wir nehmen Stichproben, falls das Bild riesig ist, für die Performance
    all_values <- (c(as.matrix(image),
                          unlist(lapply(filters, as.matrix))))

    full_range <- range(all_values, na.rm = TRUE)

    # 2. DER TRICK: Wir erstellen 256 Farbstufen ("breaks"),
    # die sich an der Verteilung der Daten orientieren.
    # So werden feine Unterschiede dort sichtbar, wo viele Daten sind.
    color_breaks <- quantile(all_values, probs = seq(0, 1, length.out = 257), na.rm = TRUE)

    # Doppelte Breaks verhindern (kann bei sehr homogenen Flächen passieren)
    color_breaks <- unique(color_breaks)
    if(length(color_breaks) < 2) color_breaks <- seq(full_range[1], full_range[2], length.out = 257)

    my_cols <- viridis::viridis(length(color_breaks) - 1)

    n <- length(filters) + 1
    rows <- 2
    cols <- ceiling(n / rows)
    par(mfrow = c(rows, cols), mar = c(3,3,3,1),mgp = c(0.6, 0, 0))

    # Hilfsfunktion zum Plotten mit fixem Bereich
    plot_fixed <- function(img_data, title) {
      log_img <- (img_data)
      if (inherits(img_data, "SpatRaster") || inherits(img_data, "RasterLayer")) {
        image(log_img, main=title, col=my_cols, breaks=color_breaks, axes=FALSE,useRaster=TRUE)
      } else {
        image(log_img, main=title, col=my_cols, breaks=color_breaks,
              axes=FALSE, useRaster=TRUE)
      }
    }

    # Original Plotten
    plot_fixed(image, "Original")

    # Filter Plotten
    for (name in names(filters)) {
      plot_fixed(filters[[name]], paste(name, "Filter"))
    }
    # 3. Legende (Adaptiv an die Quantile angepasst)
    plot.new()
    # Wir nutzen 0 bis 1 als Koordinaten für die Logik
    plot.window(xlim=c(0,1), ylim=c(0,1))

    # Der Farbbalken bleibt optisch linear (0 bis 1)
    rasterImage(as.raster(rev(viridis::viridis(256))), 0.3, 0, 0.5, 1)

    # Wir wählen 5 repräsentative Indizes aus deinen color_breaks (z.B. 0%, 25%, 50%, 75%, 100%)
    break_indices <- round(seq(1, length(color_breaks), length.out = 5))
    tick_positions <- seq(0, 1, length.out = 5) # Wo sie am Balken (0 bis 1) sitzen
    tick_labels <- round(color_breaks[break_indices], 2) # Die echten Werte an diesen Stellen

    # Achse zeichnen (ohne labels)
    axis(4, at=tick_positions, labels=FALSE, pos=0.55, tcl = -0.2)

    # Beschriftung rechts daneben
    text(x = 0.6,
         y = tick_positions,
         labels = tick_labels,
         adj = 0,
         xpd = TRUE,
         cex = 1.2)

    mtext("Scala (adaptiv)", side=3, at=0.4, line=1, font=2)# 3. Legende (Adaptiv an die Quantile angepasst)
  }
  return(filters)
}
