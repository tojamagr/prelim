
# Function to find correlated variables
find_cor <- function(data, threshold) {

  # Get column names
  cols <- names(data)

  # Initialize empty list
  correlated <- list()

  # Iterate through columns
  for (i in 1:(length(cols)-1)) {

    for (j in (i+1):length(cols)) {

      # Calculate correlation
      corr <- cor(data[,i], data[,j])

      # Check if above threshold
      if(sqrt((corr)^2) > threshold) {

        # Add to list
        correlated[[paste0(cols[i], "_", cols[j])]] <- corr

      }

    }

  }

  # Return list of correlated variables
  return(correlated)

}
