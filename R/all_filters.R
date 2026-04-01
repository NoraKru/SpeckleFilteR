#' All Filters (mean, median, kuan and lee) for Speckle Noise
#'
#' Applies all filters to a grayscale image/raster to reduce speckle noise.
#'
#' @param image A numeric matrix, raster::RasterLayer, or terra::SpatRaster representing the image.
#' @param window_size An integer specifying the size of the window. Default is 3.
#' @param ENL Effective Number of Looks.
#' @param plot_result If TRUE, plots Original + filtered images.
#' @return A named list with filtered images: $lee, $kuan, $mean, $median
#' @export

all_filters <- function(image, window_size = 3, ENL = NULL, plot_result = TRUE) {

  #filter image with all four filter
  lee_res    <- lee_filter(image, window_size = window_size, ENL = ENL)
  kuan_res   <- kuan_filter(image, window_size = window_size, ENL = ENL)
  mean_res   <- mean_filter(image, window_size = window_size)
  median_res <- median_filter(image, window_size = window_size)

  #write the results in one list
  filters <- list(
    lee    = lee_res,
    kuan   = kuan_res,
    mean   = mean_res,
    median = median_res
  )

  if (plot_result) {

    # Combine all image values (original and filtered) into a single vector
    # to calculate a common scale for direct comparison.
    all_values <- (c(as.matrix(image),
                          unlist(lapply(filters, as.matrix))))

    full_range <- range(all_values, na.rm = TRUE)

    # Calculate 256 adaptive color breaks based on data quantiles.
    # This enhances local contrast in high-density data regions.
    color_breaks <- quantile(all_values, probs = seq(0, 1, length.out = 257), na.rm = TRUE)

    # Ensure unique breaks to avoid errors in homogeneous areas
    color_breaks <- unique(color_breaks)
    if(length(color_breaks) < 2) color_breaks <- seq(full_range[1], full_range[2], length.out = 257)

    # Define color palette based on the number of valid breaks
    my_cols <- viridis::viridis(length(color_breaks) - 1)

    # Set up plot grid layout (e.g., 2 rows, multiple columns)
    n <- length(filters) + 1
    rows <- 2
    cols <- ceiling(n / rows)
    par(mfrow = c(rows, cols), mar = c(3,3,3,1),mgp = c(0.6, 0, 0))

    # Internal helper function for consistent plotting across all filter outputs
    plot_fixed <- function(img_data, title) {
      log_img <- (img_data)
      if (inherits(img_data, "SpatRaster") || inherits(img_data, "RasterLayer")) {
        image(log_img, main=title, col=my_cols, breaks=color_breaks, axes=FALSE,useRaster=TRUE)
      } else {
        image(log_img, main=title, col=my_cols, breaks=color_breaks,
              axes=FALSE, useRaster=TRUE)
      }
    }

    # Plot the original image
    plot_fixed(image, "Original")

    # Plot each filtered result
    for (name in names(filters)) {
      plot_fixed(filters[[name]], paste(name, "Filter"))
    }

    #Adaptive Legend Construction
    plot.new()
    # Define a normalized coordinate system (0 to 1) for the legend
    plot.window(xlim=c(0,1), ylim=c(0,1))

    # Render a linear color bar representing the viridis scale
    rasterImage(as.raster(rev(viridis::viridis(256))), 0.3, 0, 0.5, 1)

    # Extract 5 representative labels from the adaptive color breaks (0%, 25%, 50%, 75%, 100%)
    break_indices <- round(seq(1, length(color_breaks), length.out = 5))
    tick_positions <- seq(0, 1, length.out = 5) # Wo sie am Balken (0 bis 1) sitzen
    tick_labels <- round(color_breaks[break_indices], 2) # Die echten Werte an diesen Stellen

    # Draw axis ticks on the right side of the color bar
    axis(4, at=tick_positions, labels=FALSE, pos=0.55, tcl = -0.2)

    # Place labels with left-alignment (adj=0) for consistent formatting
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
