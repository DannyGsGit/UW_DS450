#### Random Forest Model ####
## This model is trained with cleansed, but not feature-engineered data 
## to set a baseline performance.


library(randomForest)
library(dplyr)


set.seed(206)



#~~~~~~~~~~~~~~~~~~~~~~~
#### Import dataset ####
#~~~~~~~~~~~~~~~~~~~~~~~

load("./Data/Processed/2_ProcessedData.RData")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Define target & Features ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## We will train on log(SalePrice), so need to add that column
my.data$log.SalePrice <- log(my.data$SalePrice)


#~~~~~~~~~~~~~~~~~~~~~~~
#### Cross-validation ####
#~~~~~~~~~~~~~~~~~~~~~~~

## Break out features and target for cross-validation
features <- my.data %>% select(MSSubClass.cutoff,
                               MSZoning.cutoff, LotFrontage,
                               LotArea, LotShape.cutoff, LotConfig,
                               Neighborhood.cutoff,HouseStyle.cutoff,
                               OverallQual, OverallCond, YearBuilt,
                               RoofStyle.cutoff, Exterior1st.cutoff, MasVnrType.cutoff,
                               MasVnrArea, BsmtQual, BsmtExposure.cutoff,
                               BsmtFinType1, BsmtFinSF1, BsmtUnfSF, X1stFlrSF,
                               X2ndFlrSF, LowQualFinSF, GrLivArea, BsmtFullBath,
                               BsmtHalfBath, FullBath, HalfBath, BedroomAbvGr,
                               KitchenAbvGr, KitchenQual.cutoff, TotRmsAbvGrd,
                               Functional.cutoff, Fireplaces, FireplaceQu.cutoff,
                               GarageType.cutoff, GarageFinish, GarageCars,
                               GarageQual.cutoff, GarageCond, WoodDeckSF, OpenPorchSF,
                               EnclosedPorch, X3SsnPorch, ScreenPorch, PoolArea,
                               MiscVal, MoSold, YrSold, SaleType.cutoff, SaleCondition.cutoff)
target <- my.data$log.SalePrice


# ## Run the cross-validation
# cv.training <- rfcv(train.features, train.target, cv.fold = 10)
# 
# ## Plot the cv error by number of vars
# with(cv.training, plot(n.var, error.cv, type="b", col="red"))


#~~~~~~~~~~~~~~~~~~~~~~~
#### Train ####
#~~~~~~~~~~~~~~~~~~~~~~~

# Split the data
splits <- caret::createDataPartition(y = my.data$log.SalePrice, p = 0.7, list = FALSE)

train.features <- features[splits,]
train.target <- target[splits]

test.features <- features[-splits,]
test.target <- target[-splits]


# Train
fit <- randomForest(x = train.features, y = train.target,
                    importance = TRUE,
                    ntree = 100)


## View variable importance
feature.importance <- as.data.frame(fit$importance)









#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Prediction & Evaluation ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test.data <- test.features
test.data$pred.log.SalePrice <- predict(fit, test.data)
test.data$log.SalePrice <- test.target
test.data <- test.data %>% mutate(pred.SalePrice = exp(pred.log.SalePrice),
                                  SalePrice = exp(log.SalePrice),
                                  residuals = SalePrice - pred.SalePrice)

RMSE <- sqrt(mean(test.data$residuals ^ 2))
print(RMSE)






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Generate Kaggle Submission ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Import test dataset
competition.data <- read.csv("./Data/Raw/test.csv")

## Tidy the data
source("./Code/Modeling/Data_Cleanser.R")
competition.data <- clean_dataset(competition.data)

## Get categorical columns and compare to training levels
model.levels <- fit$forest$xlevels


# ## Inspect dataset
# # Build a summary table to target complex categorical columns
# data.types <- sapply(competition.data, class)
# 
# # Investigate missing data
# na.investigate <- apply(competition.data, 2, function(x) length(which(is.na(x))))
# 
# # Build summary table
# data.summary.table <- data.frame(type = data.types, factor.levels = NA, na.count = na.investigate)
# data.summary.table$factor.levels <- sapply(competition.data, function(x) length(levels(x)))
# 
# 
# ## Run the prediction
# competition.data$log.SalePrice <- predict(fit, competition.data)







