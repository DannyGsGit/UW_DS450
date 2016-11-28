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

## We will train on log(SalePrice), so need to add that column
my.data$log.SalePrice <- log(my.data$SalePrice)


#~~~~~~~~~~~~~~~~~~~~~~~
#### Cross-validation ####
#~~~~~~~~~~~~~~~~~~~~~~~

## Break out features and target for cross-validation
features <- my.data %>% select(GrLivArea, OverallQual, YearBuilt, X1stFlrSF,
                               KitchenQual, Neighborhood, GarageCars, BsmtFinType1,
                               MSSubClass, BsmtQual, X2ndFlrSF, LotArea,
                               GarageType, FireplaceQu, GarageFinish, OverallCond,
                               Fireplaces, Exterior1st, FullBath, BsmtFinType1,
                               LotFrontage, TotRmsAbvGrd, MSZoning, BsmtUnfSF,
                               OpenPorchSF, BedroomAbvGr, HouseStyle, BsmtExposure,
                               WoodDeckSF, MasVnrArea, HalfBath, GarageQual,
                               BsmtFullBath, GarageCond, SaleCondition, LotShape,
                               MasVnrType, KitchenAbvGr, ScreenPorch, Functional)
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
splits <- caret::createDataPartition(y = my.data$log.SalePrice, p = 0.8, list = FALSE)

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
save(feature.importance, file = "./Docs/Models/Itr3/VariableImportance.RData")

# Plot importance
feature.importance$feature <- row.names(feature.importance)
colnames(feature.importance) <- c("PctIncMSE", "GiniImpurity", "Feature")
feature.importance$Feature <- factor(feature.importance$Feature, levels = feature.importance$Feature[order(feature.importance$PctIncMSE)])

feature.importance.plot <- ggplot(feature.importance, aes(Feature, PctIncMSE)) +
  geom_bar(stat = "identity") +
  coord_flip()
feature.importance.plot






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Prediction & Evaluation ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test.data <- test.features
test.data$pred.log.SalePrice <- predict(fit, test.data)
test.data$log.SalePrice <- test.target
test.data <- test.data %>% mutate(pred.SalePrice = exp(pred.log.SalePrice),
                                  SalePrice = exp(log.SalePrice),
                                  residuals = SalePrice - pred.SalePrice)

# Calculate RMSE
RMSE <- sqrt(mean(test.data$residuals ^ 2))
print(RMSE)

# Plot Residuals
qplot(test.data$SalePrice, test.data$pred.SalePrice)
qplot(test.data$SalePrice, test.data$residuals/test.data$SalePrice)

# Investigate records with largest error
# GrLivArea, OverallQual, YearBuilt, X1stFlrSF,
# KitchenQual, Neighborhood, GarageCars, BsmtFinType1,
# MSSubClass, BsmtQual, X2ndFlrSF, LotArea,
# GarageType, FireplaceQu, GarageFinish, OverallCond,
# Fireplaces, Exterior1st, FullBath, BsmtFinType1,
# LotFrontage, TotRmsAbvGrd, MSZoning, BsmtUnfSF,
# OpenPorchSF, BedroomAbvGr, HouseStyle, BsmtExposure,
# WoodDeckSF, MasVnrArea, HalfBath, GarageQual,
# BsmtFullBath, GarageCond, SaleCondition, LotShape,
# MasVnrType, KitchenAbvGr, ScreenPorch, Functional
test.data <- test.data %>% mutate(pct.residuals = abs(residuals/SalePrice),
                                  outlier = ifelse(pct.residuals > 0.25, "TRUE", "FALSE"))

# lattice::stripplot(SalePrice ~ Neighborhood, groups = factor(outlier), data = test.data, auto.key = list(columns = 2))
# lattice::xyplot(SalePrice ~ GrLivArea, groups = factor(outlier), data = test.data, auto.key = list(columns = 2))
# lattice::xyplot(SalePrice ~ OverallQual, groups = factor(outlier), data = test.data, auto.key = list(columns = 2))
# lattice::xyplot(SalePrice ~ X1stFlrSF, groups = factor(outlier), data = test.data, auto.key = list(columns = 2))
# lattice::xyplot(pct.residuals ~ TotRmsAbvGrd, groups = factor(outlier), data = test.data, auto.key = list(columns = 2))
# lattice::stripplot(SalePrice ~ SaleCondition, groups = factor(outlier), data = test.data, auto.key = list(columns = 2))
# lattice::histogram(~ Neighborhood|outlier, data = test.data)
# lattice::xyplot(SalePrice ~ YearBuilt, groups = factor(outlier), data = test.data, auto.key = list(columns = 2))

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

## Are all features in the model?
## New Data summary table
data.summary <- data.frame(class = sapply(competition.data, class))
data.summary$feature <- row.names(data.summary)
data.summary$in.model <- ifelse(data.summary$feature %in% names(fit$forest$xlevels), TRUE, FALSE)
## For factor features in the model, check that levels match
factor.features <- data.summary %>% filter(in.model == TRUE, class == "factor")
factor.features$level.match <- FALSE
for (i in 1:nrow(factor.features)) {
  current.feature <- factor.features$feature[i]
  zz.1 <- levels(competition.data[,current.feature])
  zz.2 <- as.character(unlist(fit$forest$xlevels[current.feature]))
  factor.features$level.match[i] <- identical(zz.1, zz.2)
}





## Run the prediction
competition.data <- competition.data %>% select(Id,
                                                GrLivArea, OverallQual, YearBuilt, X1stFlrSF,
                                                KitchenQual, Neighborhood, GarageCars, BsmtFinType1,
                                                MSSubClass, BsmtQual, X2ndFlrSF, LotArea,
                                                GarageType, FireplaceQu, GarageFinish, OverallCond,
                                                Fireplaces, Exterior1st, FullBath, BsmtFinType1,
                                                LotFrontage, TotRmsAbvGrd, MSZoning, BsmtUnfSF,
                                                OpenPorchSF, BedroomAbvGr, HouseStyle, BsmtExposure,
                                                WoodDeckSF, MasVnrArea, HalfBath, GarageQual,
                                                BsmtFullBath, GarageCond, SaleCondition, LotShape,
                                                MasVnrType, KitchenAbvGr, ScreenPorch, Functional)


competition.data$log.SalePrice <- predict(fit, competition.data)

competition.data <- competition.data %>% mutate(SalePrice = exp(competition.data$log.SalePrice)) %>%
  select(Id, SalePrice)

## Save the results
write.csv(competition.data, file = "./Data/Submissions/Itr3_RF_Submission.csv",
          row.names = FALSE)




