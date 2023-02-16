# Devnotes:
# O Needs to handle non-numeric input, data-wrangling
# O Needs to round numbers

prelim <- function(data, digs = 2){
  if(is.null(dim(data)) == TRUE){
    Rows = NROW(data)

    Skew = skew(data)

    Mean = mean(data, na.rm = T)
    SD = sd(data, na.rm = T)

    Median = median(data, na.rm = T)
    IQR = IQR(data, na.rm = T)

    oput <- cbind(Rows, Skew, Mean, SD, Median, IQR)
    output <- data.frame(oput)

    } else {
      Skew = lapply(data, skew)

      Mean = lapply(data, mean)
      SD = lapply(data, sd)

      Median = lapply(data, median)
      IQR = lapply(data, IQR)

      oput <- cbind(colnames(data), Skew, Mean, SD, Median, IQR)
      output <- data.frame(oput, row.names = TRUE)
  }
  output[] <- lapply(output, function(x) if(is.numeric(x)) round(x, digits = digs) else as.character(x))
  return(output)
}

