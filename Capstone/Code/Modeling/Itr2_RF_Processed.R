#### Iteration 2- Random Forest Model ####
## This is a basic random forest, trained with processed data that has been:
## * All missing values removed
## * For factors, levels with small numbers of samples (typ < 3% of total) are re-labeled "Other"
## * Obviously information-deficient features have been dropped


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
features <- my.data %>% select(MSSubClass,
                               MSZoning, LotFrontage,
                               LotArea, LotShape, LotConfig,
                               Neighborhood,HouseStyle,
                               OverallQual, OverallCond, YearBuilt,
                               RoofStyle, Exterior1st, MasVnrType,
                               MasVnrArea, BsmtQual, BsmtExposure,
                               BsmtFinType1, BsmtFinSF1, BsmtUnfSF, X1stFlrSF,
                               X2ndFlrSF, LowQualFinSF, GrLivArea, BsmtFullBath,
                               BsmtHalfBath, FullBath, HalfBath, BedroomAbvGr,
                               KitchenAbvGr, KitchenQual, TotRmsAbvGrd,
                               Functional, Fireplaces, FireplaceQu,
                               GarageType, GarageFinish, GarageCars,
                               GarageQual, GarageCond, WoodDeckSF, OpenPorchSF,
                               EnclosedPorch, X3SsnPorch, ScreenPorch, PoolArea,
                               MiscVal, MoSold, YrSold, SaleType, SaleCondition)
target <- my.data$log.SalePrice


# ## Run the cross-validation
# cv.training <- rfcv(features, target, cv.fold = 10)
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
                    ntree = 700)


## View variable importance
feature.importance <- as.data.frame(fit$importance)
save(feature.importance, file = "./Docs/Models/Itr2/VariableImportance.RData")








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

source("./Code/Modeling/Data_Processor.R")
competition.data <- align_factors_with_model(model = fit, new.data = competition.data)

## Last check, look for NAs in competition data
data.types <- sapply(competition.data, class)
na.investigate <- apply(competition.data, 2, function(x) length(which(is.na(x))))
data.summary.table <- data.frame(type = data.types, factor.levels = NA, na.count = na.investigate)
data.summary.table$factor.levels <- sapply(competition.data, function(x) length(levels(x)))
data.summary.table$in.model <- ifelse(row.names(data.summary.table) %in% names(fit$forest$xlevels), TRUE, FALSE)
# Check that all model features are in place
model.check <- data.frame(feature = names(fit$forest$xlevels))
model.check$in.new.data <- ifelse(model.check$feature %in% colnames(competition.data), TRUE, FALSE)

## Run the prediction
competition.data <- competition.data %>% select(Id, MSSubClass,
                               MSZoning, LotFrontage,
                               LotArea, LotShape, LotConfig,
                               Neighborhood,HouseStyle,
                               OverallQual, OverallCond, YearBuilt,
                               RoofStyle, Exterior1st, MasVnrType,
                               MasVnrArea, BsmtQual, BsmtExposure,
                               BsmtFinType1, BsmtFinSF1, BsmtUnfSF, X1stFlrSF,
                               X2ndFlrSF, LowQualFinSF, GrLivArea, BsmtFullBath,
                               BsmtHalfBath, FullBath, HalfBath, BedroomAbvGr,
                               KitchenAbvGr, KitchenQual, TotRmsAbvGrd,
                               Functional, Fireplaces, FireplaceQu,
                               GarageType, GarageFinish, GarageCars,
                               GarageQual, GarageCond, WoodDeckSF, OpenPorchSF,
                               EnclosedPorch, X3SsnPorch, ScreenPorch, PoolArea,
                               MiscVal, MoSold, YrSold, SaleType, SaleCondition)


competition.data$log.SalePrice <- predict(fit, competition.data)

competition.data <- competition.data %>% mutate(SalePrice = exp(competition.data$log.SalePrice)) %>%
  select(Id, SalePrice)

## Save the results
write.csv(competition.data, file = "./Data/Submissions/Itr2_RF_Submission.csv",
          row.names = FALSE)




