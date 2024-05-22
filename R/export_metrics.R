#' Export Metrics (Reliability & Validity)
#' 
#' @description
#' 
#' A function to export a \code{metrics} object as a formatted excel workbook to
#' the specified \code{file} path. The format is consistent with what is
#' typically reported in APA 7th edition, and can be copy and pasted directly
#' into a Word document or similar document software.
#'
#' @param metrics A required list object. The list should contain an array for
#'   composite scores under \code{data}, a data frame of indicator loadings and
#'   weights under \code{metrics}, and composite validity value under
#'   \code{validity}.
#' @param digits The decimal places for the metrics to be rounded to. Default is
#'   3.
#' @param file A required file path. If specified, the results will be written
#'   as a formatted excel workbook.
#'
#' @return A formatted excel workbook saved to specified file path.
#' 
#' @export
export_metrics <- function(
    metrics,
    digits,
    file
) {
  
  # Set decimal place parameter
  decimal_places = paste0("0.",
                          paste(rep("0",
                                    digits),
                                collapse = ""))
  
  
  # Separate working data frames
  df <- metrics[["data"]]
  mt <- metrics[["metrics"]]
  vl <- metrics[["validity"]]
  
  # Create a new workbook
  wb <- openxlsx::createWorkbook()
  
  # Add worksheets to the workbook
  openxlsx::addWorksheet(wb, "Data")
  openxlsx::addWorksheet(wb, "Metrics")
  openxlsx::addWorksheet(wb, "Validity")
  
  
  
  # -- WRITING DATA -- #
  
  # Write the data to the worksheet
  openxlsx::writeData(wb, "Data", df)
  
  # Create a style for the header: bold and border
  headerStyle <- openxlsx::createStyle(textDecoration = "bold",
                                       border = "topBottom")
  
  # Apply the style to the header
  openxlsx::addStyle(wb,
                     "Data",
                     style = headerStyle,
                     rows = 1,
                     cols = 1:ncol(df),
                     gridExpand = TRUE)
  
  
  
  # -- WRITING METRICS -- #
  
  # Rename columns
  mt <- mt %>% 
    dplyr::rename("Composite" = composite,
                  "Indicator" = indicator,
                  "Loadings"  = loadings,
                  "Weights"   = weights) %>% 
    
    # Replace "_" with space
    dplyr::mutate(Composite = gsub("_", " ", Composite)) %>% 
    
    # Apply title capitalization
    dplyr::mutate(Composite = stringr::str_to_title(Composite))
  
  # Write metrics to the worksheet
  openxlsx::writeData(wb, "Metrics", mt)
  
  
  
  # Create a style for the header: bold, border, and center
  headerStyle_r1  <- openxlsx::createStyle(textDecoration = "bold",
                                           border = "topBottom",
                                           halign = "center")
  
  # Create a border style for borderline at bottom of cell
  borderStyle     <- openxlsx::createStyle(border = "bottom")
  
  # Create a style for rounding cells to k decimal places
  roundStyle      <- openxlsx::createStyle(numFmt = decimal_places,
                                           halign = "center")
  
  # Create a style for rounding cells to k decimal places with bottom border
  roundStyle_r1   <- openxlsx::createStyle(numFmt = decimal_places,
                                           halign = "center",
                                           border = "bottom")
  
  
  # Apply the style to the header (row 1, column 1)
  openxlsx::addStyle(wb,
                     "Metrics",
                     style = headerStyle,
                     rows = 1,
                     cols = 1,
                     gridExpand = TRUE)
  
  # Apply the style to the header (row 1, column 2:n)
  openxlsx::addStyle(wb,
                     "Metrics",
                     style = headerStyle_r1,
                     rows = 1,
                     cols = 2:ncol(mt),
                     gridExpand = TRUE)
  
  # Apply the style to the columns "Loadings" and "Weights"
  openxlsx::addStyle(wb,
                     "Metrics",
                     style = roundStyle,
                     rows = 2:(nrow(mt)),
                     cols = 3:4,
                     gridExpand = TRUE)
  
  # Apply the style to the columns "Loadings" and "Weights"
  openxlsx::addStyle(wb,
                     "Metrics",
                     style = roundStyle_r1,
                     rows = nrow(mt)+1,
                     cols = 3:4,
                     gridExpand = TRUE)
  
  # Apply the border style to the last row
  openxlsx::addStyle(wb,
                     "Metrics",
                     style = borderStyle,
                     rows = nrow(mt)+1,
                     cols = 1:2,
                     gridExpand = TRUE)
  
  
  # Calculate the maximum string length in the first column
  mt_max_length_c1 <- max(nchar(as.character(mt[,1])))
  mt_max_length_c2 <- max(nchar(as.character(mt[,2])))
  
  # Set the width of the first column based on the maximum string length
  openxlsx::setColWidths(wb,
                         "Metrics",
                         cols = 1,
                         widths = mt_max_length_c1)
  
  openxlsx::setColWidths(wb,
                         "Metrics",
                         cols = 2,
                         widths = mt_max_length_c2)
  
  
  
  # -- WRITING VALIDITY -- #
  
  # Create a border style for borderline at bottom of cell
  borderStyle_cen <- openxlsx::createStyle(border = "bottom",
                                           halign = "center")
  
  # Create a style for centering horizontally
  cellStyle <- openxlsx::createStyle(halign = "center")
  
  
  # Rename columns
  vl <- vl %>% 
    dplyr::rename("Composite" = composite,
                  "\u03B1" = alpha,        # Cronbach's alpha
                  "\u03C1c" = rhoc,        # Composite reliability rho c
                  "AVE" = ave,
                  "\u03BB \u2208" = loading_range,
                  "W \u2208" = weight_range) %>% 
    
    # Replace _ with space
    dplyr::mutate(Composite = gsub("_", " ", Composite)) %>% 
    
    # Apply capitalization
    dplyr::mutate(Composite = stringr::str_to_title(Composite))
  
  # Write the dataframe to the worksheet
  openxlsx::writeData(wb, "Validity", vl)
  
  
  # Apply the style to the header (row 1, column 1)
  openxlsx::addStyle(wb,
                     "Validity",
                     style = headerStyle,
                     rows = 1,
                     cols = 1,
                     gridExpand = TRUE)
  
  # Apply the style to the header (row 1, column 2:n)
  openxlsx::addStyle(wb,
                     "Validity",
                     style = headerStyle_r1,
                     rows = 1,
                     cols = 2:ncol(vl),
                     gridExpand = TRUE)
  
  # Apply the style to the columns "alpha", "rhoc", and "ave"
  openxlsx::addStyle(wb,
                     "Validity",
                     style = roundStyle,
                     rows = 2:(nrow(vl)),
                     cols = 2:4,
                     gridExpand = TRUE)
  
  # Apply the style to the columns "alpha", "rhoc", and "ave"
  openxlsx::addStyle(wb,
                     "Validity",
                     style = roundStyle_r1,
                     rows = nrow(vl)+1,
                     cols = 2:4,
                     gridExpand = TRUE)
  
  # Apply centering style to columns of loading and weight ranges
  openxlsx::addStyle(wb,
                     "Validity",
                     style = cellStyle,
                     rows = 2:nrow(vl),
                     cols = 5:6,
                     gridExpand = TRUE)
  
  # Apply the border style to the last row
  openxlsx::addStyle(wb,
                     "Validity",
                     style = borderStyle,
                     rows = nrow(vl)+1,
                     cols = 1,
                     gridExpand = TRUE)
  
  openxlsx::addStyle(wb,
                     "Validity",
                     style = borderStyle_cen,
                     rows = nrow(vl)+1,
                     cols = 5:6,
                     gridExpand = TRUE)
  
  
  # Calculate the maximum string length in the first column
  vl_max_length <- max(nchar(as.character(vl[,1])))
  vl_lmb_length <- max(nchar(as.character(vl[,5])))
  vl_wgt_length <- max(nchar(as.character(vl[,6])))
  
  # Set the width of the columns based on the maximum string length
  openxlsx::setColWidths(wb,
                         "Validity",
                         cols = 1,
                         widths = vl_max_length)
  
  openxlsx::setColWidths(wb,
                         "Validity",
                         cols = 5,
                         widths = vl_lmb_length)
  
  openxlsx::setColWidths(wb,
                         "Validity",
                         cols = 6,
                         widths = vl_wgt_length)
  
  # Write "Hello World" to the first column and row after the last row of the table
  openxlsx::writeData(wb,
                      "Validity",
                      x = "Note: \u03B1 = Cronbach's alpha; \u03C1c = Composite reliability; AVE = Average Variance Extracted; \u03BB \u2208 = Loadings Range; W \u2208 = Weights Range.",
                      startCol = 1,
                      startRow = nrow(vl) + 2)
  
  
  
  
  # -- SAVE FILE -- #
  openxlsx::saveWorkbook(wb,
                         file,
                         overwrite = TRUE)
  
  
}
