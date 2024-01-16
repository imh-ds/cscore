#' Calculate Distance-to-Median Composite Score with Decay Function
#' 
#' @description
#' Create a composite score by specifying the indicators that go into
#' its respective composite variable. Composite scores are weighted based on 
#' the indicators' distances to the median of all indicators. Indicators 
#' with greater distance from the median (i.e., greater the absolute difference
#' between the median and the indicator) are downweighted based on a decay
#' function.
#' 
#' In a decay function, weights decrease exponentially as the distance from 
#' the median increases. The rate of decrease is controlled by the decay rate. 
#' A higher decay rate results in a faster decrease, in which values further 
#' from the median are less influential on the composite calculation. This
#' function is particularly sensitive to the decay rate parameter.
#' 
#' Values close to the median (i.e., small distances) will have weights 
#' approaching 1, whereas values further from the median (i.e., large 
#' distances) will have weights approaching 0. The rate at which the weights 
#' decrease as the distance increases is determined by the `decay_rate`.
#' A larger `decay_rate` results in a faster decrease (i.e., weights will 
#' approach 0 more quickly), whereas a smaller `decay_rate` results in a slower 
#' decrease (i.e., weights will stay closer to 1 for larger distances).
#' 
#' The `decay_rate` therefore controls the weighting schema's sensitivity to the
#' distance from the median. A higher `decay_rate` increases the influence of 
#' indicators closer to the median. A lower `decay_rate` mutes the effect of
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
#' @param decay_rate A numeric value reflecting the decay rate 
#' (i.e., sensitivity) of the distance-to-median weighting schema. The default
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
#' calc_dismed_decay_composite(data = grit, var = extraversion, decay_rate = 0.5)
#' 
#' @export
calc_dismed_decay_composite <- function(data,
                                        var,
                                        decay_rate = 0.5){
  
  
  # Subset the dataframe to include only the specified variables
  df <- data[, var]
  
  # Calculate the median of each row
  medians <- apply(df, 1, median, na.rm = TRUE)
  
  # Calculate the distance of each variable from the median
  distances <- sweep(df, 1, medians, FUN = "-")
  
  # Calculate the weights based on the distance from the median using exponential decay function
  weights <- exp(-decay_rate * abs(distances))
  
  # Normalize weights
  normalized_weights <- weights / rowMeans(weights,
                                           na.rm = T)
  
  # Calculate the weighted composite variable
  rowMeans(df * normalized_weights,
           na.rm = TRUE)
  
  
}
