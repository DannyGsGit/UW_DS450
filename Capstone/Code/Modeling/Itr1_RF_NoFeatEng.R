#### Random Forest Model ####
## This model is trained with cleansed, but not feature-engineered data 
## to set a baseline performance.


library(randomForest)
library(dplyr)


set.seed(206)



#~~~~~~~~~~~~~~~~~~~~~~~
#### Import dataset ####
#~~~~~~~~~~~~~~~~~~~~~~~

load("./Data/Processed/1_Post_Cleansing.RData")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Define target ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## We will train on log(SalePrice), so need to add that column
my.data$log.SalePrice <- log(my.data$SalePrice)


#~~~~~~~~~~~~~~~~~~~~~~~
#### Cross-validation ####
#~~~~~~~~~~~~~~~~~~~~~~~

## Break out features and target for cross-validation
features <- my.data %>% select(-SalePrice, -log.SalePrice)
target <- my.data$log.SalePrice

## Run the cross-validation
# cv.result <- rfcv(features, target, cv.fold = 10)

## Plot the cv error by number of vars
# with(cv.result, plot(n.var, error.cv, type="b", col="red"))

## CV plot shows drop in performance between 39 and 80 features

#~~~~~~~~~~~~~~~~~~~~~~~
#### Train ####
#~~~~~~~~~~~~~~~~~~~~~~~

# Split the data
splits <- caret::createDataPartition(y = my.data$log.SalePrice, p = 0.7, list = FALSE)
train.data <- my.data[splits,]
test.data <- my.data[-splits,]

# Train


fit <- randomForest(log.SalePrice ~ MSSubClass + MSZoning + LotFrontage +
                      LotArea + Street + Alley + LotShape + LandContour +
                      Utilities + LotConfig + LandSlope + Neighborhood +
                      Condition1 + Condition2 + BldgType + HouseStyle +
                      OverallQual + OverallCond + YearBuilt + YearRemodAdd +
                      RoofStyle + RoofMatl + Exterior1st + Exterior2nd +
                      MasVnrType + ExterQual + ExterCond + Foundation +
                      BsmtQual + BsmtCond + BsmtExposure + BsmtFinType1 +
                      BsmtFinSF1 + BsmtUnfSF + TotalBsmtSF + Heating + HeatingQC +
                      CentralAir + Electrical + X1stFlrSF + X2ndFlrSF +
                      LowQualFinSF + GrLivArea + BsmtFullBath + BsmtHalfBath +
                      FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr +
                      KitchenQual + TotRmsAbvGrd + Functional + Fireplaces +
                      FireplaceQu + GarageType + GarageFinish + GarageCars +
                      GarageArea + GarageQual + GarageCond + PavedDrive +
                      WoodDeckSF + OpenPorchSF + EnclosedPorch + X3SsnPorch +
                      ScreenPorch + PoolArea + PoolQC + Fence + MiscFeature +
                      MiscVal + MoSold + YrSold + SaleType + SaleCondition,
                    data = train.data,
                    importance = TRUE,
                    ntree = 400)

## View variable importance
feature.importance <- as.data.frame(fit$importance)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Prediction & Evaluation ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test.data$pred.log.SalePrice <- predict(fit, test.data)
test.data <- test.data %>% mutate(pred.SalePrice = exp(pred.log.SalePrice),
                                  residuals = SalePrice - pred.SalePrice)

RMSE <- sqrt(mean(test.data$residuals ^ 2))




