#### Iteration 3- Random Forest Model ####
## This random forest builds upon iteration 2, with feature selection driven
## by the variable importance output of the model fit in iteration 2.


library(randomForest)
library(dplyr)
library(ggplot2)


set.seed(206)



#~~~~~~~~~~~~~~~~~~~~~~~
#### Import dataset ####
#~~~~~~~~~~~~~~~~~~~~~~~

load("./Data/Processed/2_ProcessedData.RData")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Define target & Features ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## Break out features
feature.list <- c("GrLivArea", "OverallQual", "YearBuilt", "X1stFlrSF",
                  "KitchenQual", "Neighborhood", "GarageCars", "BsmtFinType1",
                  "MSSubClass", "BsmtQual", "X2ndFlrSF", "LotArea",
                  "GarageType", "FireplaceQu", "GarageFinish", "OverallCond",
                  "Fireplaces", "Exterior1st", "FullBath",
                  "LotFrontage", "TotRmsAbvGrd", "MSZoning", "BsmtUnfSF",
                  "OpenPorchSF", "BedroomAbvGr", "HouseStyle", "BsmtExposure",
                  "WoodDeckSF", "MasVnrArea", "HalfBath", "GarageQual",
                  "BsmtFullBath", "GarageCond", "SaleCondition", "LotShape",
                  "MasVnrType", "KitchenAbvGr", "ScreenPorch", "Functional")
features <- my.data[, feature.list]


## We will train on log(SalePrice), so need to add that column
my.data$log.SalePrice <- log(my.data$SalePrice)
target <- my.data$log.SalePrice




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Feature Engineering ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~







#~~~~~~~~~~~~~~~~~~~~~~~
#### Cross-validation ####
#~~~~~~~~~~~~~~~~~~~~~~~

# ## Run the cross-validation
# cv.training <- rfcv(features, target, cv.fold = 10)
# 
# ## Plot the cv error by number of vars
# with(cv.training, plot(n.var, error.cv, type="b", col="red"))





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Generate Train/Test Splits ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Split the data
splits <- caret::createDataPartition(y = my.data$log.SalePrice, p = 0.7, list = FALSE)

train.features <- features[splits,]
train.target <- target[splits]

test.features <- features[-splits,]
test.target <- target[-splits]



#~~~~~~~~~~~~~~~~~~~~~~
#### Random Forest ####
#~~~~~~~~~~~~~~~~~~~~~~

### Train

fit <- randomForest(x = train.features, y = train.target,
                    importance = TRUE,
                    ntree = 500)


# ## View variable importance
# feature.importance <- as.data.frame(fit$importance)
# save(feature.importance, file = "./Docs/Models/Itr4/VariableImportance.RData")
# 
# # Plot importance
# feature.importance$feature <- row.names(feature.importance)
# colnames(feature.importance) <- c("PctIncMSE", "GiniImpurity", "Feature")
# feature.importance$Feature <- factor(feature.importance$Feature, levels = feature.importance$Feature[order(feature.importance$PctIncMSE)])
# 
# feature.importance.plot <- ggplot(feature.importance, aes(Feature, PctIncMSE)) +
#   geom_bar(stat = "identity") +
#   coord_flip()
# feature.importance.plot


### Test

test.data <- test.features
test.data$pred.log.SalePrice <- predict(fit, test.data)
test.data$log.SalePrice <- test.target
test.data <- test.data %>% mutate(pred.SalePrice = exp(pred.log.SalePrice),
                                  SalePrice = exp(log.SalePrice),
                                  residuals = SalePrice - pred.SalePrice)


# Calculate RMSE
RMSE.rf <- sqrt(mean(test.data$residuals ^ 2))
print(RMSE.rf)

# test.data <- test.data %>% mutate(pct.residuals = abs(residuals/SalePrice),
#                                   outlier = ifelse(pct.residuals > 0.25, "TRUE", "FALSE"))





#~~~~~~~~~~~~~~~~~~~~~~
#### xgBoost ####
#~~~~~~~~~~~~~~~~~~~~~~

library(xgboost)

### Train model
xgb.train <- data.matrix(train.features)
xgb.label <- data.matrix(train.target)

model.xgboost <- xgboost(data = xgb.train, label = xgb.label,
                         nrounds = 2,
                         booster = "gbtree",
                         objective = "reg:linear",
                         nthread = 3,
                         max_depth = 15)

### Test model
xgb.test <- data.matrix(test.features)
preds <- predict(model.xgboost, xgb.test)

xgb.results <- data.frame(test.features, test.target, preds)
xgb.results <- xgb.results %>% mutate(SalePrice = exp(test.target),
                                      pred.SalePrice = exp(preds),
                                      residuals = pred.SalePrice - SalePrice,
                                      pct.residuals = residuals / SalePrice)

# Calculate RMSE
RMSE.xgb <- sqrt(mean(xgb.results$residuals ^ 2))
print(RMSE.xgb)

xgb.plot.tree(feature_names = colnames(xgb.train), model = model.xgboost)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Generate Kaggle Submission ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Import test dataset
competition.data <- read.csv("./Data/Raw/test.csv")

## Tidy the data
source("./Code/Modeling/Data_Cleanser.R")
competition.data <- clean_dataset(competition.data)

## Process factor levels (re-bin small levels)
source("./Code/Modeling/Data_Processor.R")
competition.data <- align_factors_with_model(model = fit, new.data = competition.data)

## Generate schema for the training set
source("./Code/Modeling/Feature_Engineering_Functions.R")
model.schema <- capture_schema(features)

# Check that all model features are in place
model.check <- check_schema(new.data = competition.data, model.schema = model.schema)



## Last check, look for NAs in competition data
data.types <- sapply(competition.data, class)
na.investigate <- apply(competition.data, 2, function(x) length(which(is.na(x))))
data.summary.table <- data.frame(type = data.types, na.count = na.investigate)
data.summary.table$in.model <- ifelse(row.names(data.summary.table) %in% names(fit$forest$xlevels), TRUE, FALSE)









## Run the prediction
competition.data <- competition.data[, feature.list]


competition.data$log.SalePrice <- predict(fit, competition.data)

competition.data <- competition.data %>% mutate(SalePrice = exp(competition.data$log.SalePrice)) %>%
  select(Id, SalePrice)

## Save the results
write.csv(competition.data, file = "./Data/Submissions/Itr3_RF_Submission.csv",
          row.names = FALSE)




