#### Processes data per ./Code/Processing/2_FeatureProcessing.R ####
source("./Code/Processing/Processing_Functions.R")


align_factors_with_model <- function(model, new.data) {

  ## New Data summary table
  data.summary <- data.frame(class = sapply(new.data, class))
  data.summary$feature <- row.names(data.summary)
  data.summary$in.model <- ifelse(data.summary$feature %in% names(model$forest$xlevels), TRUE, FALSE)
  
  ## For factor features in the model, check that levels match
  factor.features <- data.summary %>% filter(in.model == TRUE, class == "factor")
  
  for (i in 1:nrow(factor.features)) {
    current.feature <- factor.features$feature[i]
    other.flag <- new.data[,current.feature] %in% unlist(model$forest$xlevels[current.feature])
    new.data[,current.feature] <- as.character((new.data[,current.feature]))
    new.data[which(other.flag==FALSE), current.feature] <- "Other"
    new.data[,current.feature] <- as.factor(new.data[,current.feature])
  }

  return(new.data)
}

