

#### Plot Histogram w/ Density ####
library(ggplot2)
plot_distribution <- function(dataset, target, label) {
  # dataset: Dataset to containing the target column, a dataframe
  # target: The column containing target feature, use dataframe$target format
  
  plot.result <- ggplot(dataset, aes(target)) +
    geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
    geom_density(alpha=.2, fill="#FF6666") +
    labs(title = paste("Histogram for", label),
         x = label)
  
  return(plot.result)
}



#### Amend NA values in a column with a new value ####
amend_na <- function(column.name, new.level = "None") {
  levels(column.name) <- c(levels(column.name), new.level)
  column.name[is.na(column.name)] <- new.level
  return(column.name)
}


#### Cutoff factor levels by a minimum % of representation ####
cutoff_by_size <- function(data.column, cutoff = 0.1) {
  # Get number of records by factor level, then scale
  factor.level.sizes <- table(data.column)
  factor.level.sizes <- factor.level.sizes / sum(factor.level.sizes)
  
  # Keep only levels above some threshold
  keep.factors <- levels(data.column)[which(factor.level.sizes > cutoff)]
  
  cutoff <- ifelse(data.column %in% keep.factors, as.character(data.column), "Other")
  
  return(cutoff)
}