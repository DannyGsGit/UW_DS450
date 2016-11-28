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





#### 1- Neighborhood binned by saleprice ####
neighborhood.plot <- ggplot(my.data, aes(x = Neighborhood, y = log.sale.price)) +
  geom_boxplot()
neighborhood.plot

# Summary stats by neighborhood
mean.log.sale <- mean(my.data$log.sale.price)
neighborhood.summary <- my.data %>% group_by(Neighborhood) %>%
  summarise(mean.log.price = mean(log.sale.price) - mean.log.sale,
            sd.log.price = sd(log.sale.price),
            count = n()) %>%
  ungroup()

# Group neighborhoods by sale price
neighborhood.quantiles <- quantile(neighborhood.summary$mean.log.price, probs = c(seq(0, 1, 0.15), 1))
neighborhood.summary$neighborhood.binned <- cut(neighborhood.summary$mean.log.price, neighborhood.quantiles, include.lowest = TRUE)

neighborhood.plot.2 <- ggplot(neighborhood.summary, aes(Neighborhood, mean.log.price)) +
  geom_bar(stat = "identity", aes(fill = neighborhood.binned))
neighborhood.plot.2

# Apply groupings to dataset
neighborhood.summary <- neighborhood.summary %>% select(neighborhood.binned, Neighborhood)
my.data <- left_join(my.data, neighborhood.summary, by = "Neighborhood")





#### 1.1- Simply cut-off neighborhoods by # of sales ####

my.data$Neighborhood.cutoff <- cutoff_by_size(my.data$Neighborhood, cutoff = co.level) %>% as.factor()
lattice::histogram(my.data$Neighborhood.cutoff)





#### 2- Bin Condition 1 ####
# Set categories by positive and negative conditions
positive.conditions <- c("PosA", "PosN")
negative.conditions <- c("Artery", "Feedr", "RRAe", "RRAn", "RRNe", "RRNn")

# Apply bins
my.data <- my.data %>% mutate(Condition1.binned = ifelse(Condition1 %in% positive.conditions, 1,
                                                         ifelse(Condition1 %in% negative.conditions, -1, 0)))

# Inspect results
lattice::histogram(~Condition1, data = my.data)
lattice::histogram(~Condition1.binned, data = my.data)






#### 3- MSSubClass ####
lattice::histogram(~MSSubClass, data = my.data)

mssub.plot <- ggplot(my.data, aes(x = MSSubClass, y = log.sale.price)) +
  geom_boxplot()
mssub.plot

mean.log.sale <- mean(my.data$log.sale.price)
mssub.summary <- my.data %>% group_by(MSSubClass) %>%
  summarise(mean.log.price = mean(log.sale.price) - mean.log.sale,
            sd.log.price = sd(log.sale.price),
            count = n()) %>%
  ungroup()

# Break up the MSSubClasses by mean log price
mssub.quantiles <- quantile(mssub.summary$mean.log.price, probs = c(seq(0, 1, 0.15), 1))
mssub.summary$mssub.binned <- cut(mssub.summary$mean.log.price, mssub.quantiles, include.lowest = TRUE)

# Look at the groupings
mssub.summary.plot <- ggplot(mssub.summary, aes(MSSubClass, mean.log.price)) +
  geom_bar(stat = "identity", aes(fill = mssub.binned))
mssub.summary.plot

# Apply groupings to original dataset
mssub.summary <-  mssub.summary %>% select(MSSubClass, mssub.binned)
my.data <- left_join(my.data, mssub.summary, by = "MSSubClass")


#### 3.1- MSSubClass ####
# lattice::histogram(my.data$MSSubClass)
my.data$MSSubClass.cutoff <- cutoff_by_size(my.data$MSSubClass, cutoff = co.level) %>% as.factor()
# lattice::histogram(my.data$MSSubClass.cutoff)







#### 4- Alley ####
my.data <- my.data %>% mutate(alley.binned = ifelse(Alley == "None", "None", "Alley"))
my.data$alley.binned <- as.factor(my.data$alley.binned)




#### 5- Exterior1st ####
# lattice::histogram(my.data$Exterior1st)
my.data$Exterior1st.cutoff <- cutoff_by_size(my.data$Exterior1st, cutoff = co.level) %>% as.factor()
# lattice::histogram(my.data$Exterior1st.cutoff)




#### 6- SaleType ####
# lattice::histogram(my.data$SaleType)
my.data$SaleType.cutoff <- cutoff_by_size(my.data$SaleType, cutoff = co.level) %>% as.factor()
# lattice::histogram(my.data$SaleType.cutoff)




#### 7- HouseStyle ####
# lattice::histogram(my.data$HouseStyle)
my.data$HouseStyle.cutoff <- cutoff_by_size(my.data$HouseStyle, cutoff = co.level) %>% as.factor()
# lattice::histogram(my.data$HouseStyle.cutoff)




#### 8- Functional ####
# lattice::histogram(my.data$Functional)
my.data$Functional.cutoff <- cutoff_by_size(my.data$Functional, cutoff = co.level) %>% as.factor()
# lattice::histogram(my.data$Functional.cutoff)


#### 9- GarageType ####
# lattice::histogram(my.data$GarageType)
my.data$GarageType.cutoff <- cutoff_by_size(my.data$GarageType, cutoff = co.level) %>% as.factor()
# lattice::histogram(my.data$GarageType.cutoff)



#### 10- RoofStyle ####
# lattice::histogram(my.data$RoofStyle)
my.data$RoofStyle.cutoff <- cutoff_by_size(my.data$RoofStyle, cutoff = co.level) %>% as.factor()
# lattice::histogram(my.data$RoofStyle.cutoff)



#### 11- FireplaceQu ####
#lattice::histogram(my.data$FireplaceQu)
my.data$FireplaceQu.cutoff <- cutoff_by_size(my.data$FireplaceQu, cutoff = co.level) %>% as.factor()
#lattice::histogram(my.data$FireplaceQu.cutoff)




#### 12- GarageQual ####
# lattice::histogram(my.data$GarageQual)
my.data$GarageQual.cutoff <- cutoff_by_size(my.data$GarageQual, cutoff = .05) %>% as.factor()
# lattice::histogram(my.data$GarageQual.cutoff)




#### 13- SaleCond ####
# lattice::histogram(my.data$SaleCondition)
my.data$SaleCondition.cutoff <- cutoff_by_size(my.data$SaleCondition, cutoff = co.level) %>% as.factor()
# lattice::histogram(my.data$SaleCondition.cutoff)





#### 14- MSZoning ####
# lattice::histogram(my.data$MSZoning)
my.data$MSZoning.cutoff <- cutoff_by_size(my.data$MSZoning, cutoff = .05) %>% as.factor()
# lattice::histogram(my.data$MSZoning.cutoff)



# #### 15- BsmtQual ####  [[[Deprecated]]]
# lattice::histogram(my.data$BsmtQual)
# my.data$BsmtQual.cutoff <- cutoff_by_size(my.data$BsmtQual, cutoff = co.level) %>% as.factor()
# lattice::histogram(my.data$BsmtQual.cutoff)



#### 16- BsmtExposure ####
# lattice::histogram(my.data$BsmtExposure)
my.data$BsmtExposure.cutoff <- cutoff_by_size(my.data$BsmtExposure, cutoff = co.level) %>% as.factor()
# lattice::histogram(my.data$BsmtExposure.cutoff)



#### 17- LotShape ####
# lattice::histogram(my.data$LotShape)
my.data$LotShape.cutoff <- cutoff_by_size(my.data$LotShape, cutoff = co.level) %>% as.factor()
# lattice::histogram(my.data$LotShape.cutoff)



#### 18- MasVnrType ####
# lattice::histogram(my.data$MasVnrType)
my.data$MasVnrType.cutoff <- cutoff_by_size(my.data$MasVnrType, cutoff = co.level) %>% as.factor()
# lattice::histogram(my.data$MasVnrType.cutoff)



#### 19- KitchenQual ####
# lattice::histogram(my.data$KitchenQual)
my.data$KitchenQual.cutoff <- cutoff_by_size(my.data$KitchenQual, cutoff = co.level) %>% as.factor()
# lattice::histogram(my.data$KitchenQual.cutoff)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Export Dataset for modeling ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

save(my.data, file = "./Data/Processed/2_ProcessedData.RData")
