#' Export Metrics (Reliability & Validity)
#'
#' @description A function to export a \code{metrics} object as a formatted
#'   excel workbook to the specified \code{file} path. The format is consistent
#'   with what is typically reported in APA 7th edition, and can be copy and
#'   pasted directly into a Word document or similar document software.
#'
#' @details Indicator loadings (\eqn{\lambda_i}) are computed as the bivariate
#' correlation between each indicator \eqn{I_i} and the correlation-weighted
#' composite score \eqn{\bar{C}_c}:
#'
#' \deqn{\lambda_i = \mathrm{cor}(I_i, \bar{C}_c)}
#'
#' Two reliability metrics are reported: Cronbach's alpha (\eqn{\alpha}) and
#' composite reliability (\eqn{\rho_c}, also referred to as McDonald's omega,
#' \eqn{\omega}).
#'
#' Cronbach's alpha is defined as:
#'
#' \deqn{\alpha = \frac{k \bar{c}}{\bar{v} + (k - 1)\bar{c}}}
#'
#' where \eqn{k} is the number of indicators, \eqn{\bar{c}} is the average
#' inter-item covariance, and \eqn{\bar{v}} is the average indicator variance.
#' Alpha assumes tau-equivalence (equal loadings across indicators) and is
#' sensitive to the number of indicators. While widely used, it is not
#' recommended due to these limitations; it is reported here for completeness
#' only.
#'
#' Composite reliability (\eqn{\rho_c}) is a weighted reliability estimate based
#' on user-defined indicator weights. It uses the same formula as McDonald's
#' omega but allows loadings to be weighted according to the composite score
#' structure:
#'
#' \deqn{\rho_c = \frac{\left(\sum \lambda_i w_i\right)^2}{\left(\sum \lambda_i
#' w_i\right)^2 + \sum e_{w_i}}}
#'
#' where \eqn{w_i} is the weight for indicator \eqn{i}, and \eqn{e_{w_i}} is the
#' weighted measurement error:
#'
#' \deqn{e_{w_i} = (1 - \lambda_i^2) \cdot w_i}
#'
#' Although this metric is labeled \eqn{\rho_c} to align with PLS-SEM
#' nomenclature, it is mathematically equivalent to omega and may be reported as
#' either.
#'
#' Average Variance Extracted (AVE) measures the proportion of variance captured
#' by the construct relative to measurement error. It is defined as:
#'
#' \deqn{\mathrm{AVE} = \frac{\sum \lambda_{w_i}^2}{\sum \lambda_{w_i}^2 + \sum
#' e_{w_i}}}
#'
#' where the weighted squared loading is:
#'
#' \deqn{\lambda_{w_i}^2 = \lambda_i^2 \cdot w_i}
#'
#' AVE is commonly used in the Fornell-Larcker criterion for discriminant
#' validity:
#'
#' \deqn{\sqrt{\mathrm{AVE}_i} > \max_{j} (r_{ij})}
#'
#' That is, the square root of AVE for a construct should exceed its highest
#' correlation with any other construct.
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
  # openxlsx::addWorksheet(wb,
  #                        sheetName = "Data")
  openxlsx::addWorksheet(wb,
                         sheetName = "Metrics")
  openxlsx::addWorksheet(wb,
                         sheetName = "Validity")
  
  
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
      "\u03B1" = alpha,        # Cronbach's alpha
      "\u03C1C" = rhoc,        # Composite reliability rho c
      "AVE" = ave,
      "\u03BB \u2208" = loading_range,
      "W \u2208" = weight_range
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
  
  # Apply the style to the columns "alpha", "rhoc", and "ave"
  openxlsx::addStyle(
    wb,
    sheet = "Validity",
    style = roundStyle,
    rows = (start_row + 3):(start_row + 2 + nrow(vl)),
    cols = (start_col + 1):(start_col + 3),
    gridExpand = TRUE
  )
  
  # Apply the style to the columns "alpha", "rhoc", and "ave"
  openxlsx::addStyle(
    wb,
    sheet = "Validity",
    style = roundStyle_r1,
    rows = start_row + 2 + nrow(vl),
    cols = (start_col + 1):(start_col + 3),
    gridExpand = TRUE
  )
  
  # Apply centering style to columns of loading and weight ranges
  openxlsx::addStyle(
    wb,
    sheet = "Validity",
    style = cellStyle,
    rows = (start_row + 3):(start_row + 2 + nrow(vl)),
    cols = (start_col + 4):(start_col + 5),
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
    cols = (start_col + 4):(start_col + 5),
    gridExpand = TRUE
  )
  
  
  # Calculate the maximum string length in the first column
  vl_max_length <- max(nchar(as.character(vl[,1])))
  vl_lmb_length <- max(nchar(as.character(vl[,5])))
  vl_wgt_length <- max(nchar(as.character(vl[,6])))
  
  # Set the width of the columns based on the maximum string length
  openxlsx::setColWidths(
    wb,
    "Validity",
    cols = start_col,
    widths = vl_max_length
  )
  
  openxlsx::setColWidths(
    wb,
    "Validity",
    cols = start_col + 4,
    widths = vl_lmb_length
  )
  
  openxlsx::setColWidths(
    wb,
    "Validity",
    cols = start_col + 5,
    widths = vl_wgt_length
  )
  
  # Write "Hello World" to the first column and row after the last row of the table
  openxlsx::writeData(
    wb,
    "Validity",
    x = "Note: \u03B1 = Cronbach's alpha; \u03C1c = Composite reliability; AVE = Average Variance Extracted; \u03BB \u2208 = Loadings Range; W \u2208 = Weights Range.",
    startCol = start_col,
    startRow = start_row + 3 + nrow(vl)
  )
  
  # Hide gridlines
  openxlsx::showGridLines(wb,
                          sheet = "Validity",
                          showGridLines = FALSE)
  
  
  # -- SAVE FILE -- #
  openxlsx::saveWorkbook(wb,
                         file,
                         overwrite = TRUE)
  
  
}
