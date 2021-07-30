
#' Coefficient of Variation
#'
#' @examples
#' 
#' x <- 1:10
#' x <- c(1:5, NA, 6:10)
#' coef_variation(x)
#' 
coef_variation <- function(x, na.rm = TRUE) {
  
  message("proportion of missing values: ", mean(is.na(x)))
  
  x / sum(x, na.rm = na.rm)
  
  sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
  
}


#' Variance
#' 
#' @examples
#' 
#' x <- c(1:5, NA, 6:10)
#' mean(x, na.rm = TRUE)
#' var(x, na.rm = TRUE)
#' 
var_my <- function(x) {
  stopifnot(is.numeric(x))
  
  x <- x[!is.na(x)]
  n <- length(x)
  
  one_over_n_1 <- 1 / (n - 1)
  sample_mean <- sum(x) / n
  final <- one_over_n_1 * sum((x - sample_mean) ^ 2)
  return(final)
}

