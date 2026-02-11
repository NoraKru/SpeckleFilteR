#' Evaluate Multiple Speckle Filter Quality Metrics
#'
#' Computes several evaluation metrics (MSE, Average Difference, Speckle Index,
#' PSNR, SNR) for one or multiple filtered images.
#'
#' @param image Original image (matrix or raster)
#' @param filtered_image A single filtered image or a named list of filtered images
#'
#' @return A data.frame with one row per filter and one column per metric
#' @examples
#' \dontrun{
#' img <- load_example()
#' filters <- all_filters(img, window_size = 3, ENL = 3.5, plot_result = FALSE)
#' metrics <- evaluate_all_metrics(img, filters)
#' # Ranked table of evaluation metrics
#' print(results$metrics)
#'
#' # Short textual summary
#' cat(results$summary)
#' }
#' @export
evaluate_all_metrics <- function(image, filtered_image) {

  # ---- Case 1: single filtered image → convert to list ----
  if (!is.list(filtered_image)) {
    filtered_image <- list(filter = filtered_image)
  }

  # ---- Compute evaluation metrics ----
  df <- do.call(rbind, lapply(names(filtered_image), function(name) {

    f_img <- filtered_image[[name]]

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

  # ---- Compute score (+1 for best per metric) ----
  df$score <- 0

  df$score[which.min(df$MSE)]  <- df$score[which.min(df$MSE)]  + 1
  df$score[which.min(df$AD)]   <- df$score[which.min(df$AD)]   + 1
  df$score[which.min(df$SI)]   <- df$score[which.min(df$SI)]   + 1
  df$score[which.max(df$PSNR)] <- df$score[which.max(df$PSNR)] + 1
  df$score[which.max(df$SNR)]  <- df$score[which.max(df$SNR)]  + 1

  # ---- Rank filters by total score ----
  df$rank <- rank(-df$score, ties.method = "min")
  df <- df[order(df$rank), ]
  rownames(df) <- NULL

  # ---- Short textual summary ----
  best_filter <- df$filter[1]

  summary <- paste(
    "The filter", best_filter, "achieves the best overall performance.",
    "It provides the most favorable balance between low error measures",
    "(MSE, AD, SI) and high quality measures (PSNR, SNR).",
    "The ranking is based on a simple scoring scheme across all metrics."
  )

  # ---- Print results directly ----
  cat("=== Filter Evaluation Metrics ===\n")
  print(df)
  cat("\n=== Summary ===\n")
  cat(summary_text, "\n")

  # ---- Return results invisibly ----
  invisible(list(
    metrics = df,
    summary = summary_text
  ))
}
