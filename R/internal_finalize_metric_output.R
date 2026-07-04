#' Finalize Metric Output
#'
#' @description Attach discriminant-validity outputs to the metric return
#'   object and optionally export the workbook.
#'
#' @param data Scored data frame.
#' @param metrics Indicator-level metric table.
#' @param validity Composite-level reliability/convergent-validity table.
#' @param composite_list A \code{composite_list} object.
#' @param digits Number of decimal places for export formatting.
#' @param file Optional workbook path.
#' @param name Optional analysis name.
#' @param htmt_cutoff Numeric HTMT threshold.
#'
#' @returns A list of metric outputs.
#'
#' @keywords internal
finalize_metric_output <- function(
    data,
    metrics,
    validity,
    composite_list,
    digits,
    file = NULL,
    name = NULL,
    htmt_cutoff = 0.90
) {
  discriminant_report <- calc_discriminant_validity_report(
    data = data,
    composite_list = composite_list,
    validity = validity,
    htmt_cutoff = htmt_cutoff
  )

  reporting_set <- discriminant_report[["reporting_set"]]

  validity <- validity %>%
    dplyr::left_join(
      discriminant_report[["discriminant_summary"]],
      by = "composite"
    )

  if (length(reporting_set) > 0) {
    validity <- validity %>%
      dplyr::filter(composite %in% reporting_set) %>%
      dplyr::mutate(
        composite = factor(composite, levels = reporting_set)
      ) %>%
      dplyr::arrange(composite) %>%
      dplyr::mutate(
        composite = as.character(composite)
      )
  }

  composite_sheets <- list(
    data = data,
    metrics = metrics,
    validity = validity,
    discriminant_summary = discriminant_report[["discriminant_summary"]],
    fornell_larcker = discriminant_report[["fornell_larcker"]],
    htmt = discriminant_report[["htmt"]]
  )

  if (!is.null(file)) {
    export_metrics(
      metrics = composite_sheets,
      digits = digits,
      name = name,
      file = file
    )
  }

  composite_sheets
}
