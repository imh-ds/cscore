#' Export Metrics (Reliability & Validity)
#'
#' @description Export the metric output returned by the scoring functions as a
#'   formatted Excel workbook containing one worksheet for indicator-level
#'   measurement-model results and one worksheet for composite-level
#'   reliability/convergent-validity summaries.
#'
#' @details
#'
#' \code{export_metrics()} is a presentation-layer function. It does
#' \emph{not} estimate loadings, weights, reliability coefficients, or AVE on
#' its own. Instead, it expects the already-computed output produced by the
#' scoring functions when \code{return_metrics = TRUE}, and then formats those
#' results into an Excel workbook using \pkg{openxlsx}.
#'
#' The function assumes that its \code{metrics} argument is a named list with
#' indicator-level, construct-level, and discriminant-validity components:
#'
#' \enumerate{
#'   \item \code{metrics[["metrics"]]}: an indicator-level table containing
#'   \code{composite}, \code{indicator}, \code{loadings}, and \code{weights}.
#'   \item \code{metrics[["validity"]]}: a composite-level table containing
#'   \code{composite}, \code{alpha}, \code{rhoc}, \code{ave},
#'   \code{loading_range}, \code{weight_range}, \code{sqrt_ave},
#'   \code{max_interconstruct_corr}, \code{fornell_larcker}, \code{max_htmt},
#'   \code{htmt}, and \code{discriminant_validity}.
#'   \item \code{metrics[["discriminant_summary"]]}: the reporting-set
#'   discriminant-validity summary table.
#'   \item \code{metrics[["fornell_larcker"]]}: a square reporting-set matrix
#'   whose diagonal is \eqn{\sqrt{AVE}} and whose off-diagonals are absolute
#'   inter-construct correlations.
#'   \item \code{metrics[["htmt"]]}: a square reporting-set matrix of pairwise
#'   HTMT ratios.
#' }
#'
#' These quantities are produced upstream by \code{calc_metrics()} and, for
#' single-indicator composites, by \code{calc_single_indicator()}.
#'
#' For multi-indicator composites, the exported indicator loadings are the
#' corrected item-total correlations implemented in \code{calc_metrics()},
#' namely
#'
#' \deqn{\lambda_j = \mathrm{cor}\!\left(x_j,\,
#' \frac{\sum_{k \neq j} x_k w_k^{*}}{\sum_{k \neq j}|w_k^{*}|}\right)}
#'
#' computed with pairwise-complete observations. This differs from correlating
#' each indicator with a composite that already contains that same indicator,
#' and therefore avoids part-whole inflation.
#'
#' The exported composite-level statistics are also inherited from
#' \code{calc_metrics()}. Cronbach's alpha (\eqn{\alpha}) is obtained from
#' \code{ltm::cronbach.alpha()} when \pkg{ltm} is installed. The exported
#' weighted composite reliability (\eqn{\rho_c}) is
#'
#' \deqn{\rho_c =
#' \frac{\left(\sum_{j=1}^{m}\lambda_j w_j^{*}\right)^2}
#' {\left(\sum_{j=1}^{m}\lambda_j w_j^{*}\right)^2 +
#'  \sum_{j=1}^{m}(1-\lambda_j^2)(w_j^{*})^2}}
#'
#' and the exported Average Variance Extracted (AVE) is
#'
#' \deqn{\mathrm{AVE} =
#' \frac{\sum_{j=1}^{m}\lambda_j^2 w_j^{*}}
#' {\sum_{j=1}^{m}\lambda_j^2 w_j^{*} +
#'  \sum_{j=1}^{m}(1-\lambda_j^2) w_j^{*}}}
#'
#' where \eqn{w_j^{*}} denotes the final normalized indicator-weight vector used
#' by the relevant scoring method, or the reporting-weight vector derived from
#' it in methods such as the median family.
#'
#' The function then creates a workbook with four worksheets:
#'
#' \enumerate{
#'   \item \strong{Metrics}: indicator-level measurement-model output, with
#'   columns relabeled to \code{Composite}, \code{Indicator}, \code{Loadings},
#'   and \code{Weights}.
#'   \item \strong{Validity}: composite-level output, with columns relabeled to
#'   \code{Composite}, \code{alpha}, \code{rhoc}, \code{AVE}, the
#'   discriminant-validity summary fields, \code{loading range}, and
#'   \code{weight range}.
#'   \item \strong{Fornell-Larcker}: appendix-style matrix output for the
#'   reporting set.
#'   \item \strong{HTMT}: appendix-style matrix of pairwise HTMT ratios for the
#'   reporting set.
#' }
#'
#' In both sheets, underscores in composite names are replaced by spaces and
#' then title-cased using \code{stringr::str_to_title()}. In the indicator-level
#' sheet, repeated composite labels are blanked after their first occurrence so
#' that rows visually group by construct.
#'
#' The workbook layout is fixed by the implementation:
#'
#' \enumerate{
#'   \item Titles are written starting at row 3, column 2.
#'   \item The data tables begin two rows below each title.
#'   \item Numeric columns are formatted to the requested number of decimal
#'   places via an \pkg{openxlsx} number format string constructed from
#'   \code{digits}.
#'   \item Header rows receive bold formatting and top/bottom borders.
#'   \item The last data row receives bottom-border styling.
#'   \item Range columns in the validity sheet are horizontally centered.
#'   \item Column widths for the first identifier columns and the range columns
#'   are adjusted from the maximum printed string length in those columns.
#'   \item Gridlines are hidden on both worksheets.
#' }
#'
#' The function writes a footnote on the \code{Validity} sheet explaining the
#' abbreviations used in the exported table. Finally, the workbook is saved to
#' \code{file} with \code{overwrite = TRUE}.
#'
#' @param metrics A required list object. The list should contain an array for
#'   composite scores under \code{data}, a data frame of indicator loadings and
#'   weights under \code{metrics}, and composite validity value under
#'   \code{validity}.
#' @param digits The decimal places for the metrics to be rounded to. Default is
#'   3.
#' @param name An optional string denoting the study/analysis name.
#' @param file A required file path. If specified, the results will be written
#'   as a formatted excel workbook.
#'
#' @return A formatted excel workbook saved to specified file path.
#'
#' @export
export_metrics <- function(
    
    metrics,
    digits,
    name = NULL,
    file
    
) {

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop(
      "Package 'openxlsx' is required to export metrics workbooks. ",
      "Install it with: install.packages(\"openxlsx\")",
      call. = FALSE
    )
  }

  required_elements <- c(
    "metrics", "validity", "discriminant_summary", "fornell_larcker", "htmt"
  )

  if (!is.list(metrics) || !all(required_elements %in% names(metrics))) {
    stop(
      "`metrics` must be a list containing: ",
      paste(required_elements, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  required_metric_cols <- c("composite", "indicator", "loadings", "weights")
  required_validity_cols <- c("composite", "alpha", "rhoc", "ave",
                              "loading_range", "weight_range", "sqrt_ave",
                              "max_interconstruct_corr", "fornell_larcker",
                              "max_htmt", "htmt", "discriminant_validity")

  if (!is.data.frame(metrics[["metrics"]]) ||
      !all(required_metric_cols %in% names(metrics[["metrics"]]))) {
    stop(
      "`metrics[['metrics']]` must be a data frame with columns: ",
      paste(required_metric_cols, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (!is.data.frame(metrics[["validity"]]) ||
      !all(required_validity_cols %in% names(metrics[["validity"]]))) {
    stop(
      "`metrics[['validity']]` must be a data frame with columns: ",
      paste(required_validity_cols, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (!is.matrix(metrics[["fornell_larcker"]]) ||
      !is.matrix(metrics[["htmt"]])) {
    stop(
      "`metrics[['fornell_larcker']]` and `metrics[['htmt']]` must both be matrices.",
      call. = FALSE
    )
  }
  
  # Set decimal place parameter
  decimal_places = paste0("0.",
                          paste(rep("0",
                                    digits),
                                collapse = ""))
  
  
  # Separate working data frames
  mt <- metrics[["metrics"]] %>% 
    dplyr::mutate(
      composite = ifelse(!duplicated(composite), composite, NA)
    )
    
  vl <- metrics[["validity"]]
  
  # Create a new workbook
  wb <- openxlsx::createWorkbook()
  
  # Add worksheets to the workbook
  openxlsx::addWorksheet(wb,
                         sheetName = "Metrics")
  openxlsx::addWorksheet(wb,
                         sheetName = "Validity")
  openxlsx::addWorksheet(wb,
                         sheetName = "Fornell-Larcker")
  openxlsx::addWorksheet(wb,
                         sheetName = "HTMT")
  
  
  # Starting parameters
  start_row <- 3
  start_col <- 2
  

  # STYLES ------------------------------------------------------------------

  # Create a style for the header: bold and border
  headerStyle <- openxlsx::createStyle(
    textDecoration = "bold",
    border = "topBottom"
  )
  
  # Create a style for the header: bold, border, and center
  headerStyle_r1 <- openxlsx::createStyle(
    textDecoration = "bold",
    border = "topBottom",
    halign = "center"
  )
  
  # Create a border style for borderline at bottom of cell
  borderStyle <- openxlsx::createStyle(
    border = "bottom"
  )
  
  # Create a style for rounding cells to k decimal places
  roundStyle <- openxlsx::createStyle(
    numFmt = decimal_places,
    halign = "center"
  )
  
  # Create a style for rounding cells to k decimal places with bottom border
  roundStyle_r1 <- openxlsx::createStyle(
    numFmt = decimal_places,
    halign = "center",
    border = "bottom"
  )
  
  # Create a border style for borderline at bottom of cell
  borderStyle_cen <- openxlsx::createStyle(
    border = "bottom",
    halign = "center"
  )
  
  # Create a style for centering horizontally
  cellStyle <- openxlsx::createStyle(
    halign = "center"
  )

  methodologyHeaderStyle <- openxlsx::createStyle(
    fontSize = 14,
    textDecoration = "bold"
  )


  # WRITE METRICS -----------------------------------------------------------

  # Rename columns
  mt <- mt %>% 
    dplyr::rename(
      "Composite" = composite,
      "Indicator" = indicator,
      "Loadings"  = loadings,
      "Weights"   = weights
    ) %>% 
    
    # Replace "_" with space
    dplyr::mutate(Composite = gsub("_", " ", Composite)) %>% 
    
    # Apply title capitalization
    dplyr::mutate(Composite = stringr::str_to_title(Composite))
  
  
  # Write title to metrics sheet
  openxlsx::writeData(
    wb = wb,
    sheet = "Metrics",
    x = if(!is.null(name)) {
      paste0(name,
             " - Measurement Model (Loadings & Weights)")
    } else {
      "Measurement Model (Loadings & Weights)"
    },
    startCol = start_col,
    startRow = start_row
  )
  
  # Apply format for title
  openxlsx::addStyle(
    wb = wb,
    sheet = "Metrics",
    style = openxlsx::createStyle(
      fontSize = 20,
      textDecoration = "bold"
    ),
    cols = start_col,
    rows = start_row
  )
  
  
  # Write metrics to the worksheet
  openxlsx::writeData(
    wb, 
    sheet = "Metrics", 
    x = mt,
    startCol = start_col,
    startRow = start_row + 2
  )
  
  # Apply the style to the header (row 1, column 1)
  openxlsx::addStyle(
    wb,
    sheet = "Metrics",
    style = headerStyle,
    rows = start_row + 2,
    cols = start_col,
    gridExpand = TRUE
  )
  
  # Apply the style to the header (row 1, column 2:n)
  openxlsx::addStyle(
    wb,
    sheet = "Metrics",
    style = headerStyle_r1,
    rows = start_row + 2,
    cols = (start_col + 1):(start_col + ncol(mt) - 1),
    gridExpand = TRUE
  )
  
  # Apply the style to the columns "Loadings" and "Weights"
  openxlsx::addStyle(
    wb,
    sheet = "Metrics",
    style = roundStyle,
    rows = (start_row + 3):(start_row + 2 + nrow(mt)),
    cols = (start_col + 2):(start_col + 3),
    gridExpand = TRUE
  )
  
  # Apply the style to the columns "Loadings" and "Weights"
  openxlsx::addStyle(
    wb,
    sheet = "Metrics",
    style = roundStyle_r1,
    rows = start_row + 2 + nrow(mt),
    cols = (start_col + 2):(start_col + 3),
    gridExpand = TRUE
  )
  
  # Apply the border style to the last row
  openxlsx::addStyle(
    wb,
    sheet = "Metrics",
    style = borderStyle,
    rows = start_row + 2 + nrow(mt),
    cols = start_col:(start_col + 1),
    gridExpand = TRUE
  )
  
  
  # Calculate the maximum string length in the first column
  mt_max_length_c1 <- max(nchar(as.character(mt[,1])),
                          na.rm = T)
  mt_max_length_c2 <- max(nchar(as.character(mt[,2])),
                          na.rm = T)
  
  # Set the width of the first column based on the maximum string length
  openxlsx::setColWidths(
    wb,
    sheet = "Metrics",
    cols = 2,
    widths = mt_max_length_c1
  )
  
  openxlsx::setColWidths(
    wb,
    sheet = "Metrics",
    cols = 3,
    widths = mt_max_length_c2
  )
  
  # Hide gridlines
  openxlsx::showGridLines(wb,
                          sheet = "Metrics",
                          showGridLines = FALSE)
  
  
  # WRITE VALIDITY ----------------------------------------------------------

  # Rename columns
  vl <- vl %>% 
    dplyr::rename(
      "Composite" = composite,
      "alpha" = alpha,
      "rho_c" = rhoc,
      "AVE" = ave,
      "sqrt(AVE)" = sqrt_ave,
      "max |r|" = max_interconstruct_corr,
      "FL" = fornell_larcker,
      "max HTMT" = max_htmt,
      "HTMT" = htmt,
      "DV" = discriminant_validity,
      "loading range" = loading_range,
      "weight range" = weight_range
    ) %>% 
    
    # Replace _ with space
    dplyr::mutate(Composite = gsub("_", " ", Composite)) %>% 
    
    # Apply capitalization
    dplyr::mutate(Composite = stringr::str_to_title(Composite))
  
  
  # Write title to metrics sheet
  openxlsx::writeData(
    wb = wb,
    sheet = "Validity",
    x = if(!is.null(name)) {
      paste0(name,
             " - Measurement Reliability")
    } else {
      "Measurement Reliability"
    },
    startCol = start_col,
    startRow = start_row
  )
  
  # Apply format for title
  openxlsx::addStyle(
    wb = wb,
    sheet = "Validity",
    style = openxlsx::createStyle(
      fontSize = 20,
      textDecoration = "bold"
    ),
    cols = start_col,
    rows = start_row
  )
  
  
  # Write the dataframe to the worksheet
  openxlsx::writeData(
    wb = wb, 
    sheet = "Validity", 
    x = vl,
    startCol = start_col,
    startRow = start_row + 2)

  # Replace the ASCII-safe internal headers with publication-style worksheet
  # labels after writing the table so the parsed R source stays ASCII-safe.
  validity_headers <- c(
    "Composite",
    "\u03B1",
    "\u03C1c",
    "AVE",
    "\u03BB \u2208",
    "W \u2208",
    "sqrt(AVE)",
    "max |r|",
    "FL",
    "max HTMT",
    "HTMT",
    "DV"
  )

  openxlsx::writeData(
    wb = wb,
    sheet = "Validity",
    x = as.list(validity_headers),
    startCol = start_col,
    startRow = start_row + 2,
    colNames = FALSE
  )
  
  
  # Apply the style to the header (row 1, column 1)
  openxlsx::addStyle(
    wb,
    sheet = "Validity",
    style = headerStyle,
    rows = start_row + 2,
    cols = start_col,
    gridExpand = TRUE
  )
  
  # Apply the style to the header (row 1, column 2:n)
  openxlsx::addStyle(
    wb,
    sheet = "Validity",
    style = headerStyle_r1,
    rows = start_row + 2,
    cols = (start_col + 1):(start_col + ncol(vl) - 1),
    gridExpand = TRUE
  )
  
  numeric_validity_cols <- which(vapply(vl, is.numeric, logical(1))) + start_col - 1
  centered_validity_cols <- setdiff(
    start_col:(start_col + ncol(vl) - 1),
    c(start_col, numeric_validity_cols)
  )

  # Apply the style to the numeric validity columns
  openxlsx::addStyle(
    wb,
    sheet = "Validity",
    style = roundStyle,
    rows = (start_row + 3):(start_row + 2 + nrow(vl)),
    cols = numeric_validity_cols,
    gridExpand = TRUE
  )
  
  # Apply the style to the last numeric row
  openxlsx::addStyle(
    wb,
    sheet = "Validity",
    style = roundStyle_r1,
    rows = start_row + 2 + nrow(vl),
    cols = numeric_validity_cols,
    gridExpand = TRUE
  )
  
  # Apply centering style to text validity columns beyond the identifier
  openxlsx::addStyle(
    wb,
    sheet = "Validity",
    style = cellStyle,
    rows = (start_row + 3):(start_row + 2 + nrow(vl)),
    cols = centered_validity_cols,
    gridExpand = TRUE
  )
  
  # Apply the border style to the last row
  openxlsx::addStyle(
    wb,
    sheet = "Validity",
    style = borderStyle,
    rows = start_row + 2 + nrow(vl),
    cols = start_col,
    gridExpand = TRUE
  )
  
  openxlsx::addStyle(
    wb,
    sheet = "Validity",
    style = borderStyle_cen,
    rows = start_row + 2 + nrow(vl),
    cols = centered_validity_cols,
    gridExpand = TRUE
  )
  
  openxlsx::setColWidths(
    wb,
    "Validity",
    cols = start_col:(start_col + ncol(vl) - 1),
    widths = "auto"
  )

  openxlsx::setColWidths(
    wb,
    "Validity",
    cols = c(6, 7),
    widths = 15
  )

  # Keep the footnote column at a readable fixed width rather than letting the
  # long note force the full table identifier column to become too wide.
  openxlsx::setColWidths(
    wb,
    "Validity",
    cols = start_col,
    widths = 50
  )
  
  footnote_row_1 <- start_row + 3 + nrow(vl)
  footnote_row_header <- footnote_row_1 + 2
  footnote_row_fl <- footnote_row_header + 1
  footnote_row_htmt <- footnote_row_fl + 1
  footnote_row_dv <- footnote_row_htmt + 1

  openxlsx::writeData(
    wb,
    "Validity",
    x = paste0(
      "Note: \u03B1 = Cronbach's alpha; \u03C1c = Composite reliability; ",
      "AVE = Average Variance Extracted; \u03BB \u2208 = Loadings Range; ",
      "W \u2208 = Weights Range; FL = Fornell-Larcker Criterion; ",
      "HTMT = Heterotrait-Monotrait; DV = Discriminant Validity."
    ),
    startCol = start_col,
    startRow = footnote_row_1
  )

  openxlsx::writeData(
    wb,
    "Validity",
    x = "Methodology Notes",
    startCol = start_col,
    startRow = footnote_row_header
  )

  openxlsx::addStyle(
    wb,
    "Validity",
    style = methodologyHeaderStyle,
    rows = footnote_row_header,
    cols = start_col,
    gridExpand = TRUE
  )

  openxlsx::writeData(
    wb,
    "Validity",
    x = paste0(
      "FL: PASS when sqrt(AVE) > max absolute inter-construct correlation; ",
      "otherwise FAIL."
    ),
    startCol = start_col,
    startRow = footnote_row_fl
  )

  openxlsx::writeData(
    wb,
    "Validity",
    x = paste0(
      "HTMT: PASS when max HTMT < cutoff; otherwise FAIL."
    ),
    startCol = start_col,
    startRow = footnote_row_htmt
  )

  openxlsx::writeData(
    wb,
    "Validity",
    x = paste0(
      "DV: PASS when all available discriminant-validity checks pass; ",
      "otherwise FAIL."
    ),
    startCol = start_col,
    startRow = footnote_row_dv
  )
  
  # Hide gridlines
  openxlsx::showGridLines(wb,
                          sheet = "Validity",
                          showGridLines = FALSE)


  # WRITE FORNELL-LARCKER --------------------------------------------------

  fl <- as.data.frame(metrics[["fornell_larcker"]]) %>%
    tibble::rownames_to_column(var = "Composite") %>%
    dplyr::mutate(
      Composite = gsub("_", " ", Composite),
      Composite = stringr::str_to_title(Composite)
    )

  openxlsx::writeData(
    wb = wb,
    sheet = "Fornell-Larcker",
    x = if (!is.null(name)) {
      paste0(name, " - Fornell-Larcker Matrix")
    } else {
      "Fornell-Larcker Matrix"
    },
    startCol = start_col,
    startRow = start_row
  )

  openxlsx::addStyle(
    wb = wb,
    sheet = "Fornell-Larcker",
    style = openxlsx::createStyle(
      fontSize = 20,
      textDecoration = "bold"
    ),
    cols = start_col,
    rows = start_row
  )

  openxlsx::writeData(
    wb = wb,
    sheet = "Fornell-Larcker",
    x = fl,
    startCol = start_col,
    startRow = start_row + 2,
    rowNames = FALSE
  )

  openxlsx::addStyle(
    wb,
    sheet = "Fornell-Larcker",
    style = headerStyle_r1,
    rows = start_row + 2,
    cols = start_col:(start_col + ncol(fl) - 1),
    gridExpand = TRUE
  )

  openxlsx::addStyle(
    wb,
    sheet = "Fornell-Larcker",
    style = roundStyle,
    rows = (start_row + 3):(start_row + 2 + nrow(fl)),
    cols = (start_col + 1):(start_col + ncol(fl) - 1),
    gridExpand = TRUE
  )

  openxlsx::addStyle(
    wb,
    sheet = "Fornell-Larcker",
    style = roundStyle_r1,
    rows = start_row + 2 + nrow(fl),
    cols = (start_col + 1):(start_col + ncol(fl) - 1),
    gridExpand = TRUE
  )

  openxlsx::addStyle(
    wb,
    sheet = "Fornell-Larcker",
    style = borderStyle,
    rows = start_row + 2 + nrow(fl),
    cols = start_col,
    gridExpand = TRUE
  )

  openxlsx::setColWidths(
    wb,
    "Fornell-Larcker",
    cols = start_col:(start_col + ncol(fl) - 1),
    widths = "auto"
  )

  openxlsx::setColWidths(
    wb,
    "Fornell-Larcker",
    cols = start_col,
    widths = 50
  )

  openxlsx::showGridLines(
    wb,
    sheet = "Fornell-Larcker",
    showGridLines = FALSE
  )


  # WRITE HTMT --------------------------------------------------------------

  ht <- as.data.frame(metrics[["htmt"]]) %>%
    tibble::rownames_to_column(var = "Composite") %>%
    dplyr::mutate(
      Composite = gsub("_", " ", Composite),
      Composite = stringr::str_to_title(Composite)
    )

  openxlsx::writeData(
    wb = wb,
    sheet = "HTMT",
    x = if (!is.null(name)) {
      paste0(name, " - HTMT Ratios")
    } else {
      "HTMT Ratios"
    },
    startCol = start_col,
    startRow = start_row
  )

  openxlsx::addStyle(
    wb = wb,
    sheet = "HTMT",
    style = openxlsx::createStyle(
      fontSize = 20,
      textDecoration = "bold"
    ),
    cols = start_col,
    rows = start_row
  )

  openxlsx::writeData(
    wb = wb,
    sheet = "HTMT",
    x = ht,
    startCol = start_col,
    startRow = start_row + 2,
    rowNames = FALSE
  )

  openxlsx::addStyle(
    wb,
    sheet = "HTMT",
    style = headerStyle_r1,
    rows = start_row + 2,
    cols = start_col:(start_col + ncol(ht) - 1),
    gridExpand = TRUE
  )

  openxlsx::addStyle(
    wb,
    sheet = "HTMT",
    style = roundStyle,
    rows = (start_row + 3):(start_row + 2 + nrow(ht)),
    cols = (start_col + 1):(start_col + ncol(ht) - 1),
    gridExpand = TRUE
  )

  openxlsx::addStyle(
    wb,
    sheet = "HTMT",
    style = roundStyle_r1,
    rows = start_row + 2 + nrow(ht),
    cols = (start_col + 1):(start_col + ncol(ht) - 1),
    gridExpand = TRUE
  )

  openxlsx::addStyle(
    wb,
    sheet = "HTMT",
    style = borderStyle,
    rows = start_row + 2 + nrow(ht),
    cols = start_col,
    gridExpand = TRUE
  )

  openxlsx::setColWidths(
    wb,
    "HTMT",
    cols = start_col:(start_col + ncol(ht) - 1),
    widths = "auto"
  )

  openxlsx::setColWidths(
    wb,
    "HTMT",
    cols = start_col,
    widths = 50
  )

  openxlsx::showGridLines(
    wb,
    sheet = "HTMT",
    showGridLines = FALSE
  )
  
  
  # -- SAVE FILE -- #
  openxlsx::saveWorkbook(wb,
                         file,
                         overwrite = TRUE)
  
  
}
