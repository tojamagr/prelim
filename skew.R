skew <- function(x){
  3 * ((mean(x, na.rm = TRUE) - median(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
}

