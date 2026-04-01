#' Evaluate Multiple Speckle Filter Quality Metrics
#'
#' Computes several evaluation metrics (MSE, Average Difference, Speckle Index,
#' PSNR, SNR) for one or multiple filtered images.
#'
#' @param image Original image (matrix or raster)
#' @param filtered_image A single filtered image or a named list of filtered images
#' @return A list containing a ranked data.frame of metrics and a textual summary.
#' @examples
#' \dontrun{
#' img <- load_example()
#' filters <- all_filters(img, window_size = 3, ENL = 3.5, plot_result = FALSE)
#' metrics <- evaluate_all_metrics(img, filters)
#' # Ranked table of evaluation metrics
#' print(results$metrics)
#' # Short textual summary
#' cat(results$summary)
#' }
#' @import graphics
#' @importFrom stats quantile mean sd var median rank
#' @export
evaluate_all_metrics <- function(image, filtered_image) {

  # If a single image is provided, wrap it in a list for consistent processing
  if (!is.list(filtered_image)) {
    filtered_image <- list(filter = filtered_image)
  }

  # Iterate through the list of filters and compute all five evaluation metrics
  df <- do.call(rbind, lapply(names(filtered_image), function(name) {

    f_img <- filtered_image[[name]]
    # Extract individual metrics using the previously defined evaluation functions
    data.frame(
      filter = name,
      MSE  = MSE_evaluation(image, f_img),
      AD   = AD_evaluation(image, f_img),
      SI   = SI_evaluation(image, f_img),
      PSNR = PSNR_evaluation(image, f_img),
      SNR  = SNR_evaluation(image, f_img),
      stringsAsFactors = FALSE
    )
  }))

  # Initialize a scoring column to rank the filters.
  # Points are awarded to the 'best' performer in each category.
  df$score <- 0

  # Lower values are better for error-based metrics (MSE, AD, SI)
  df$score[which.min(df$MSE)]  <- df$score[which.min(df$MSE)]  + 1
  df$score[which.min(df$AD)]   <- df$score[which.min(df$AD)]   + 1
  df$score[which.min(df$SI)]   <- df$score[which.min(df$SI)]   + 1

  # Higher values are better for quality-based metrics (PSNR, SNR)
  df$score[which.max(df$PSNR)] <- df$score[which.max(df$PSNR)] + 1
  df$score[which.max(df$SNR)]  <- df$score[which.max(df$SNR)]  + 1

  # Rank filters based on their cumulative score (descending order)
  df$rank <- rank(-df$score, ties.method = "min")
  df <- df[order(df$rank), ]
  rownames(df) <- NULL

  # Identify the top-performing filter for the summary
  best_filter <- df$filter[1]

  # Generate a concise technical summary of the results
  summary <- paste(
    "The filter", best_filter, "achieves the best overall performance.",
    "It provides the most favorable balance between low error measures",
    "(MSE, AD, SI) and high quality measures (PSNR, SNR).",
    "The ranking is based on a simple scoring scheme across all metrics."
  )

  # Print the results to the console for immediate review
  cat("=== Filter Evaluation Metrics ===\n")
  print(df)
  cat("\n=== Summary ===\n")
  cat(summary, "\n")

  # Return results as a list
  invisible(list(
    metrics = df,
    summary = summary
  ))
}
