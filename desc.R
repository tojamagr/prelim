desc <-  function(x, normal = TRUE, na.rm = FALSE){
  if(normal == TRUE){
    output <- round(c(mean(x, trim=0, na.rm),sd(x, na.rm)), 2)
  } else {
    output <- round(c(median(x, na.rm),IQR(x, na.rm)), 2)  
  }
  return(output)
}
