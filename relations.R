relations <- function(data, r = 0.2, p = 0.05, use = "everything"){
  corr_matrix <- cor(data)
  
  highly_correlated <- which(
    (corr_matrix) > r | (corr_matrix) < (-r)  &  corr_matrix != 1,
    arr.ind = TRUE
    )
  coef_list <- corr_matrix[highly_correlated] 
  
  var_names <- rownames(corr_matrix)[highly_correlated[,1]]
  
  var_names2 <- colnames(
    corr_matrix)[highly_correlated[,2]]
  
  variable_list <- cbind(
    variables = paste(
      var_names,
      var_names2,
      sep = " - "),
    coefficients = round(
      coef_list,
      digits = 3)
    )

  return(variable_list)
}

