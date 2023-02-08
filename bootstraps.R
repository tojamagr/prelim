bootstraps <- function(data, samples = 20, boots = NROW(data), na.action){
  if (NCOL(data) == 1){
    frame  <- rep(0,boots)
    for (i in 1:boots){
      sample.now <- sample(data, samples)
      frame [i] <- mean(sample.now, na.rm = T)
    }
    output <- c(frame)
    return(output)

    } else {
      secc <- seq(1:boots)
      frame  <- data.frame(matrix(nrow = boots, ncol = NCOL(data)
                              ))
      for (i in secc){
        index <- floor(runif(samples, min = 1,max = (NROW(data))))
        samp <- data[index,]
        sampmean <- sapply(samp, mean)
        frame[i,] <- sampmean
        }
      colnames(frame) <- colnames(data)
      return(frame)
    }
  }
