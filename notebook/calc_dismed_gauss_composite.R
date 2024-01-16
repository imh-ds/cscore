#' Calculate Distance-to-Median Composite Score with Gaussian Function
#' 
#' @description
#' Create a composite score by specifying the indicators that go into
#' its respective composite variable. Composite scores are weighted based on 
#' the indicators' distances to the median of all indicators. Indicators 
#' with greater distance from the median (i.e., greater the absolute difference
#' between the median and the indicator) are downweighted based on a gaussian
#' function.
#' 
#' In a Gaussian function, weights decrease in a bell curve (i.e., "Gaussian") 
#' shape as the distance from the median increases. The rate of decrease is 
#' controlled by the sigma parameter. A larger sigma value results in a wider 
#' bell curve, in which the values further from the median retain their 
#' weighting on the composite calculation. The Gaussian function therefore
#' provides a smoother decrease of weights with distance-to-median.
#' 
#' Values close to the median (i.e., small distances) will have weights 
#' approaching 1, whereas values further from the median (i.e., large 
#' distances) will have weights approaching 0. The rate at which the weights 
#' decrease as the distance increases is determined by the `sigma`.
#' A larger `sigma` results in a faster decrease (i.e., weights will 
#' approach 0 more quickly), whereas a smaller `sigma` results in a slower 
#' decrease (i.e., weights will stay closer to 1 for larger distances).
#' 
#' The `sigma` therefore controls the weighting schema's sensitivity to the
#' distance from the median. A higher `sigma` increases the influence of 
#' indicators closer to the median. A lower `sigma` mutes the effect of
#' distance to the median, thereby placing more equal weight across all values
#' regardless of their distance from the median.
#' 
#' Unlike correlation, regression, or standard deviation weighted composites, 
#' the indicators do not have a single weighting schema that is applied to all
#' indicators uniformly. Rather, each observation (i.e., respondent) has their 
#' own unique weighting schema in distance-to-median weighting as every 
#' observation likewise has its own median across the indicators.
#' 
#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param var A required vector of column names associated with the indicators
#' comprising a composite variable.
#' @param sigma A numeric value reflecting the sigma value for the Gaussian 
#' function in the distance-to-median weighting schema. The default
#' value is set to 0.5.
#' 
#' @return An array of the calculated composite score.
#' 
#' @examples
#' 
#' data(grit)
#' 
#' # Specify a vector of indicators
#' extraversion <- sprintf("e%01d", seq(10))
#' 
#' # Calculate correlation-weighted composite score
#' calc_dismed_gauss_composite(data = grit, var = extraversion, sigma = 0.5)
#' 
#' @export
calc_dismed_gauss_composite <- function(data,
                                        var,
                                        sigma = 0.5){
  
  
  # Subset the dataframe to include only the specified variables
  df <- data[, var]
  
  # Calculate the median of each row
  medians <- apply(df, 1, median, na.rm = TRUE)
  
  # Calculate the distance of each variable from the median
  distances <- sweep(df, 1, medians, FUN = "-")
  
  # Calculate the weights based on the distance from the median using gaussian function
  weights <- exp(-(distances^2) / (2 * sigma^2))
  
  # Normalize weights
  normalized_weights <- weights / rowMeans(weights,
                                           na.rm = T)
  
  # Calculate the weighted composite variable
  rowMeans(df * normalized_weights,
           na.rm = TRUE)
  
  
}
