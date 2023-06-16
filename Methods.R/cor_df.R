cor_df <- function(dataset, method = "pearson", by.var = TRUE) {
  if (by.var) 
    data.to.cor <- t(dataset[, -1])
  else 
    data.to.cor <- dataset[, -1]
  
  # Convert columns to numeric
  data.to.cor <- as.data.frame(lapply(data.to.cor, as.numeric))
  
  cor.matrix <- cor(data.to.cor, method = method)
  cor.matrix
}