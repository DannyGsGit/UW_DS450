#### Feature Engineering ####

library(dplyr)
library(ggplot2)
library(tidyr)

#~~~~~~~~~~~~~~~~~~~~~~~
#### Import dataset ####
#~~~~~~~~~~~~~~~~~~~~~~~

load("./Data/Processed/1_Post_Cleansing.RData")
source("./Code/Processing/Processing_Functions.R")







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Summary Plots of Categorical Features ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Build a summary table to target complex categorical columns
data.types <- sapply(my.data, class)

# Investigate missing data
na.investigate <- apply(my.data, 2, function(x) length(which(is.na(x))))

# Build summary table
data.summary.table <- data.frame(type = data.types, factor.levels = NA, na.count = na.investigate)
data.summary.table$factor.levels <- sapply(my.data, function(x) length(levels(x)))


# lattice::histogram(my.data$Neighborhood)  # Bin by mean sale price/ cutoff by size
# lattice::histogram(my.data$Exterior2nd)  # Drop
# lattice::histogram(my.data$MSSubClass)  # Bin by mean sale/ cutoff by size
# lattice::histogram(my.data$Exterior1st)  # Cutoff by size
# lattice::histogram(my.data$Condition1)  # Bin by pos/neg conditions
# lattice::histogram(my.data$SaleType)  # Cutoff by size
# lattice::histogram(my.data$HouseStyle)  # Cutoff by size
# lattice::histogram(my.data$BsmtFinType1)  # Leave as-is
# lattice::histogram(my.data$Functional)  # Cutoff by size (note: this will flag presence of a functional deduction)
# lattice::histogram(my.data$GarageType)  # Cutoff by size
# lattice::histogram(my.data$RoofStyle)  # Cutoff by size
# lattice::histogram(my.data$Foundation)  # Drop
# lattice::histogram(my.data$Heating)  # Drop
# lattice::histogram(my.data$FireplaceQu)  # Cutoff by size
# lattice::histogram(my.data$GarageQual)  # Cutoff by size
# lattice::histogram(my.data$GarageCond)  # Drop
# lattice::histogram(my.data$SaleCond)  # Cutoff by size
# lattice::histogram(my.data$MSZoning)  # Cutoff by size
# lattice::histogram(my.data$BldgType)  # Drop
# lattice::histogram(my.data$ExterCond)  # Drop
# lattice::histogram(my.data$BsmtQual)  # Leave as.is
# lattice::histogram(my.data$BsmtCond)  # Drop
# lattice::histogram(my.data$BsmtExposure)  # Cutoff by size
# lattice::histogram(my.data$HeatingQC)  # Drop
# lattice::histogram(my.data$Electrical)  # Drop
# lattice::histogram(my.data$Fence)  # Drop
# lattice::histogram(my.data$MiscFeature)  # Drop
# lattice::histogram(my.data$LotShape)  # Cutoff by size
# lattice::histogram(my.data$LandContour)  # Drop
# lattice::histogram(my.data$MasVnrType)  # Cutoff by size
# lattice::histogram(my.data$ExterQual)  # Drop
# lattice::histogram(my.data$KitchenQual)  # Cutoff by size
# lattice::histogram(my.data$GarageFinish)  # Keep as-is
# lattice::histogram(my.data$Alley)  # Bin by presence of alley
# lattice::histogram(my.data$LandSlope)  # Drop
# lattice::histogram(my.data$CentralAir)  # Drop



#~~~~~~~~~~~~~~~~~~~~~
#### Drop Columns ####
#~~~~~~~~~~~~~~~~~~~~~

## Drop
my.data <- my.data %>% select(-Street, -Utilities, -PavedDrive, - GarageArea,
                              -TotalBsmtSF, -Condition2, -RoofMatl, -PoolQC,
                              -BsmtFinType2, - Heating, -ExterCond, -BsmtCond,
                              -HeatingQC, - Electrical, -LandContour, -ExterQual,
                              -LandSlope, -CentralAir, - Exterior2nd, -Foundation,
                              -BldgType, -Fence, -MiscFeature, -YearRemodAdd,
                              -BsmtFinSF2)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Feature engineering ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Set standard threshold for cutoff by size
co.level <- 0.03



#### 0- Add log-price column ####
my.data <- my.data %>% mutate(log.sale.price = log(SalePrice))







#### 1.1- Simply cut-off neighborhoods by # of sales ####

my.data$Neighborhood <- cutoff_by_size(my.data$Neighborhood, cutoff = co.level) %>% as.factor()
# lattice::histogram(my.data$Neighborhood)









#### 3.1- MSSubClass ####
# lattice::histogram(my.data$MSSubClass)
my.data$MSSubClass <- cutoff_by_size(my.data$MSSubClass, cutoff = co.level) %>% as.factor()
# lattice::histogram(my.data$MSSubClass.cutoff)






#### 5- Exterior1st ####
# lattice::histogram(my.data$Exterior1st)
my.data$Exterior1st <- cutoff_by_size(my.data$Exterior1st, cutoff = co.level) %>% as.factor()
# lattice::histogram(my.data$Exterior1st.cutoff)




#### 6- SaleType ####
# lattice::histogram(my.data$SaleType)
my.data$SaleType <- cutoff_by_size(my.data$SaleType, cutoff = co.level) %>% as.factor()
# lattice::histogram(my.data$SaleType.cutoff)




#### 7- HouseStyle ####
# lattice::histogram(my.data$HouseStyle)
my.data$HouseStyle <- cutoff_by_size(my.data$HouseStyle, cutoff = co.level) %>% as.factor()
# lattice::histogram(my.data$HouseStyle.cutoff)




#### 8- Functional ####
# lattice::histogram(my.data$Functional)
my.data$Functional <- cutoff_by_size(my.data$Functional, cutoff = co.level) %>% as.factor()
# lattice::histogram(my.data$Functional.cutoff)


#### 9- GarageType ####
# lattice::histogram(my.data$GarageType)
my.data$GarageType <- cutoff_by_size(my.data$GarageType, cutoff = co.level) %>% as.factor()
# lattice::histogram(my.data$GarageType.cutoff)



#### 10- RoofStyle ####
# lattice::histogram(my.data$RoofStyle)
my.data$RoofStyle <- cutoff_by_size(my.data$RoofStyle, cutoff = co.level) %>% as.factor()
# lattice::histogram(my.data$RoofStyle.cutoff)



#### 11- FireplaceQu ####
#lattice::histogram(my.data$FireplaceQu)
my.data$FireplaceQu <- cutoff_by_size(my.data$FireplaceQu, cutoff = co.level) %>% as.factor()
#lattice::histogram(my.data$FireplaceQu.cutoff)




#### 12- GarageQual ####
# lattice::histogram(my.data$GarageQual)
my.data$GarageQual <- cutoff_by_size(my.data$GarageQual, cutoff = .05) %>% as.factor()
# lattice::histogram(my.data$GarageQual.cutoff)




#### 13- SaleCond ####
# lattice::histogram(my.data$SaleCondition)
my.data$SaleCondition <- cutoff_by_size(my.data$SaleCondition, cutoff = co.level) %>% as.factor()
# lattice::histogram(my.data$SaleCondition.cutoff)





#### 14- MSZoning ####
# lattice::histogram(my.data$MSZoning)
my.data$MSZoning <- cutoff_by_size(my.data$MSZoning, cutoff = .05) %>% as.factor()
# lattice::histogram(my.data$MSZoning.cutoff)




#### 16- BsmtExposure ####
# lattice::histogram(my.data$BsmtExposure)
my.data$BsmtExposure <- cutoff_by_size(my.data$BsmtExposure, cutoff = co.level) %>% as.factor()
# lattice::histogram(my.data$BsmtExposure.cutoff)



#### 17- LotShape ####
# lattice::histogram(my.data$LotShape)
my.data$LotShape <- cutoff_by_size(my.data$LotShape, cutoff = co.level) %>% as.factor()
# lattice::histogram(my.data$LotShape.cutoff)



#### 18- MasVnrType ####
# lattice::histogram(my.data$MasVnrType)
my.data$MasVnrType <- cutoff_by_size(my.data$MasVnrType, cutoff = co.level) %>% as.factor()
# lattice::histogram(my.data$MasVnrType.cutoff)



#### 19- KitchenQual ####
# lattice::histogram(my.data$KitchenQual)
my.data$KitchenQual <- cutoff_by_size(my.data$KitchenQual, cutoff = co.level) %>% as.factor()
# lattice::histogram(my.data$KitchenQual.cutoff)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Export Dataset for modeling ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

save(my.data, file = "./Data/Processed/2_ProcessedData.RData")
