#### Processes data per ./Code/Processing/2_FeatureProcessing.R ####
source("./Code/Processing/Processing_Functions.R")

# Set standard threshold for cutoff by size
co.level <- 0.03




#### 1.1- Neighborhood cutoff ####

new.data$Neighborhood.cutoff <- cutoff_by_size(new.data$Neighborhood, cutoff = co.level) %>% as.factor()



#### 3.1- MSSubClass ####
new.data$MSSubClass.cutoff <- cutoff_by_size(new.data$MSSubClass, cutoff = co.level) %>% as.factor()






#### 5- Exterior1st ####
new.data$Exterior1st.cutoff <- cutoff_by_size(new.data$Exterior1st, cutoff = co.level) %>% as.factor()




#### 6- SaleType ####
new.data$SaleType.cutoff <- cutoff_by_size(new.data$SaleType, cutoff = co.level) %>% as.factor()




#### 7- HouseStyle ####
new.data$HouseStyle.cutoff <- cutoff_by_size(new.data$HouseStyle, cutoff = co.level) %>% as.factor()




#### 8- Functional ####
new.data$Functional.cutoff <- cutoff_by_size(new.data$Functional, cutoff = co.level) %>% as.factor()


#### 9- GarageType ####
new.data$GarageType.cutoff <- cutoff_by_size(new.data$GarageType, cutoff = co.level) %>% as.factor()



#### 10- RoofStyle ####
new.data$RoofStyle.cutoff <- cutoff_by_size(new.data$RoofStyle, cutoff = co.level) %>% as.factor()



#### 11- FireplaceQu ####
new.data$FireplaceQu.cutoff <- cutoff_by_size(new.data$FireplaceQu, cutoff = co.level) %>% as.factor()




#### 12- GarageQual ####
new.data$GarageQual.cutoff <- cutoff_by_size(new.data$GarageQual, cutoff = .05) %>% as.factor()




#### 13- SaleCond ####
new.data$SaleCondition.cutoff <- cutoff_by_size(new.data$SaleCondition, cutoff = co.level) %>% as.factor()





#### 14- MSZoning ####
new.data$MSZoning.cutoff <- cutoff_by_size(new.data$MSZoning, cutoff = .05) %>% as.factor()



#### 16- BsmtExposure ####
new.data$BsmtExposure.cutoff <- cutoff_by_size(new.data$BsmtExposure, cutoff = co.level) %>% as.factor()



#### 17- LotShape ####
new.data$LotShape.cutoff <- cutoff_by_size(new.data$LotShape, cutoff = co.level) %>% as.factor()



#### 18- MasVnrType ####
new.data$MasVnrType.cutoff <- cutoff_by_size(new.data$MasVnrType, cutoff = co.level) %>% as.factor()



#### 19- KitchenQual ####
new.data$KitchenQual.cutoff <- cutoff_by_size(new.data$KitchenQual, cutoff = co.level) %>% as.factor()
