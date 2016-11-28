#### Data cleansing script ####
## This script encapsulates the data cleansing operations
## outlined in 1_Exploration.R

library(dplyr)
source("./Code/Processing/Processing_Functions.R")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Clean Missing Values ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_dataset <- function(new.data) {
  # Manually set column typing on specific columns:
  factor.columns <- c("MSSubClass")
  new.data[, factor.columns] <- sapply(new.data[, factor.columns], as.factor)
  
  
  
  
  
  # Re-label NA records
  amend.cols <- c("PoolQC", "MiscFeature", "Alley", "Fence", "FireplaceQu",
                  "GarageFinish", "GarageQual", "GarageCond", "GarageType",
                  "BsmtExposure", "BsmtFinType1", "BsmtFinType2", "BsmtQual",
                  "BsmtCond", "MasVnrType")
  new.data[,amend.cols] <- sapply(new.data[,amend.cols], amend_na)
  
  
  
  
  
  # Impute missing values
  new.data$LotFrontage[is.na(new.data$LotFrontage)] <- mean(new.data$LotFrontage, na.rm = TRUE)
  new.data$Electrical[is.na(new.data$Electrical)] <- "SBrkr"  # This is the most common electrical type
  new.data$MasVnrArea[is.na(new.data$MasVnrArea)] <- 0
  

  
  
  
  
  
  # Re-set classes
  data.types <- sapply(new.data, class)
  char.columns <- colnames(new.data[,which(data.types == "character")])
  new.data[, char.columns] <- lapply(new.data[, char.columns], as.factor)
  
  
  ## Return
  return(new.data)
}





