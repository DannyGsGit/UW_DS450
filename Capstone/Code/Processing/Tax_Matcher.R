
library(dplyr)


#~~~~~~~~~~~~~~~~~~~~~~~
#### Import dataset ####
#~~~~~~~~~~~~~~~~~~~~~~~

# Original dataset
load("./Data/Processed/2_ProcessedData.RData")

# Import tax records
tax.data <- read.csv("./Data/Raw/AmesTaxRecords.csv", stringsAsFactors = FALSE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Test new matching scheme ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tax_matcher <- function(tax.dataset, target.record, return.column) {
  tax.temp <- tax.dataset %>% mutate(Error.YearBuilt = abs(YearBuilt - target.record$YearBuilt),
                                  Error.LotArea = abs(LotArea - target.record$LotArea),
                                  Error.GrLivArea = abs(GrLivArea - target.record$GrLivArea),
                                  Error.Neighborhood = ifelse(Neighborhood == target.record$Neighborhood, 0, 1)) %>%
    mutate(Error.YearBuilt = scale(Error.YearBuilt, center = TRUE, scale = TRUE),
           Error.LotArea = scale(Error.LotArea, center = TRUE, scale = TRUE),
           Error.GrLivArea = scale(Error.GrLivArea, center = TRUE, scale = TRUE),
           Error.Neighborhood = ifelse(sum(Error.Neighborhood) == nrow(tax.dataset), Error.Neighborhood, scale(Error.Neighborhood, center = TRUE, scale = TRUE)))
  
  tax.temp$Error.Score <- tax.temp$Error.YearBuilt + tax.temp$Error.LotArea + tax.temp$Error.GrLivArea + tax.temp$Error.Neighborhood
  
  tax.match <- tax.temp[which.min(tax.temp$Error.Score), return.column]
  
  return(tax.match)
}
# 
# 
# zz <- t(sapply(1:nrow(my.data), function(x) tax_matcher(tax.data, my.data[x, ])))
# 
# 
# write.csv(zz, file = "taxmatches.csv")
# 
# 
# 
# zz <- data.frame(name = as.character(unique(my.data$Neighborhood)), match = as.character(unique(my.data$Neighborhood)) %in% unique(tax.data$Neighborhood))
# 
# zz <- data.frame(name = as.character(unique(tax.data$Neighborhood)), match = as.character(unique(tax.data$Neighborhood)) %in% as.character(unique(my.data$Neighborhood)))
# 
