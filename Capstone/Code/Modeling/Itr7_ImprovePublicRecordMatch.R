#### Iteration 6- Random Forest Model ####
## This random forest builds upon iteration 2, with feature selection driven
## by the variable importance output of the model fit in iteration 2.


library(randomForest)
library(dplyr)
library(ggplot2)


set.seed(1234)



#~~~~~~~~~~~~~~~~~~~~~~~
#### Import dataset ####
#~~~~~~~~~~~~~~~~~~~~~~~

# Original dataset
load("./Data/Processed/2_ProcessedData.RData")

# Import tax records
tax.data <- read.csv("./Data/Raw/AmesTaxRecords.csv", stringsAsFactors = FALSE)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Define target & Features ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## Break out features
feature.list <- c("GrLivArea", "OverallQual", "YearBuilt", "X1stFlrSF",
                  "KitchenQual", "Neighborhood", "GarageCars", "BsmtFinType1",
                  "MSSubClass", "BsmtQual", "LotArea",
                  "GarageType", "FireplaceQu", "GarageFinish", "OverallCond",
                  "Fireplaces", "Exterior1st", "FullBath",
                  "LotFrontage", "TotRmsAbvGrd", "MSZoning", "BsmtUnfSF",
                  "OpenPorchSF", "BedroomAbvGr", "HouseStyle", "BsmtExposure",
                  "WoodDeckSF", "MasVnrArea", "HalfBath", "GarageQual",
                  "BsmtFullBath", "GarageCond", "SaleCondition", "LotShape",
                  "MasVnrType", "KitchenAbvGr", "ScreenPorch", "Functional")
# Temp Drop: "X2ndFlrSF"

features <- my.data[, feature.list]


## We will train on log(SalePrice), so need to add that column
my.data$log.SalePrice <- log(my.data$SalePrice)
target <- my.data$log.SalePrice




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Feature Engineering ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
qqplot_pairs <- function(column) {
  par(mfrow = c(1,3))
  car::qqPlot(column, main = "Raw")
  car::qqPlot(log(column), main = "LogN")
  car::qqPlot(1 / column, main = "Reciprocal")
  par(mfrow = c(1,1))
  hist(column, breaks = 30)
}

## Log transform some features

# qqplot_pairs(features$GrLivArea)
# qqplot_pairs(features$X1stFlrSF)
# qqplot_pairs(features$LotArea)
# qqplot_pairs(features$TotRmsAbvGrd)



source("./Code/Processing/Tax_Matcher.R")

# Generate lookup table of tax assessment adjustments by neighborhood
# e.g.: Homes sell for 1.2X tax assessment in neighborhood Z
features.temp <- sapply(1:nrow(my.data), function(x) tax_matcher(tax.data, features[x, ], "AssessedValue"))
features.temp <- data.frame(features, est.2015.tax.valuation = features.temp)

tax.temp.data <- data.frame(my.data, tax.value = features.temp$est.2015.tax.valuation)
neighborhood.tax.betas <- tax.temp.data %>% group_by(Neighborhood) %>%
  summarise(mean.neighborhood.beta = mean(SalePrice / tax.value)) %>%
  ungroup()


# Adjust tax valuation by neighborhood multiplier
neighborhood_tax_adjust <- function(feature.dataset, neighborhood.betas) {
  feature.temp <- left_join(feature.dataset, neighborhood.betas, by = "Neighborhood")
  
  feature.temp$adj.tax.value <- feature.temp$mean.neighborhood.beta * feature.temp$est.2015.tax.valuation
  
  feature.dataset$adj.tax.value <- feature.temp$adj.tax.value
  
  return(feature.dataset)
}


feature_engineering <- function(feature.matrix, neighborhood.ref.data, tax.dataset, neighborhood.tax.betas) {
  
  ## Add tax valuation
  tax.values <- sapply(1:nrow(feature.matrix), function(x) tax_matcher(tax.dataset, feature.matrix[x, ], "AssessedValue"))
  feature.matrix<- data.frame(feature.matrix, est.2015.tax.valuation = tax.values)

  
  ## Adjusted tax valuation
  feature.matrix <- neighborhood_tax_adjust(feature.matrix, neighborhood.tax.betas)
  feature.matrix <- feature.matrix %>% select(-est.2015.tax.valuation)
  feature.matrix$log.adj.tax.value <- log(feature.matrix$adj.tax.value)
  feature.matrix$adj.tax.value <- NULL
  
  
  ## Log transforms
  feature.matrix <- feature.matrix %>% mutate(GrLivArea = log(GrLivArea),
                                              X1stFlrSF = log(X1stFlrSF),
                                              LotArea = log(LotArea),
                                              TotRmsAbvGrd = log(TotRmsAbvGrd))
  
  
  
  
  ## Lot size adjusted by neighborhood
  neighborhood.lot.sizes <- neighborhood.ref.data %>% group_by(Neighborhood) %>%
    summarise(mean.nbhd.log.lot.size = mean(log(LotArea))) %>%
    ungroup()
  
  feature.matrix <- left_join(feature.matrix, neighborhood.lot.sizes, by = "Neighborhood")
  
  feature.matrix <- feature.matrix %>% mutate(neighborhood.lot.pctile = LotArea / mean.nbhd.log.lot.size) %>%
    select(-mean.nbhd.log.lot.size)
  
  
  
  ## Lot coverage
  feature.matrix <- feature.matrix %>% mutate(lot.coverage = exp(X1stFlrSF) / exp(LotArea))
  
  
  
  return(feature.matrix)
}

features <- feature_engineering(feature.matrix = features,
                                neighborhood.ref.data = my.data,
                                tax.dataset = tax.data,
                                neighborhood.tax.betas = neighborhood.tax.betas)


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
splits <- caret::createDataPartition(y = my.data$log.sale.price, p = 0.96, list = FALSE)

train.features <- features[splits,]
train.target <- target[splits]

test.features <- features[-splits,]
test.target <- target[-splits]



#~~~~~~~~~~~~~~~~~~~~~~
#### Random Forest ####
#~~~~~~~~~~~~~~~~~~~~~~

### Train

model.rf <- randomForest(x = train.features, y = train.target,
                         importance = TRUE,
                         ntree = 1000)


## View variable importance
feature.importance <- as.data.frame(model.rf$importance)
# save(feature.importance, file = "./Docs/Models/Itr4/VariableImportance.RData")

# Plot importance
feature.importance$feature <- row.names(feature.importance)
colnames(feature.importance) <- c("PctIncMSE", "GiniImpurity", "Feature")
feature.importance$Feature <- factor(feature.importance$Feature, levels = feature.importance$Feature[order(feature.importance$PctIncMSE)])

feature.importance.plot <- ggplot(feature.importance, aes(Feature, PctIncMSE)) +
  geom_bar(stat = "identity") +
  coord_flip()
feature.importance.plot


### Test

test.data <- test.features
test.data$log.SalePrice.RF <- predict(model.rf, test.data)
test.data$log.SalePrice <- test.target
test.data <- test.data %>% mutate(SalePrice = exp(log.SalePrice),
                                  SalePrice.RF = exp(log.SalePrice.RF),
                                  residuals.RF = SalePrice - SalePrice.RF)


# Calculate RMSE
RMSE.rf <- sqrt(mean(test.data$residuals.RF ^ 2))
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
                         nrounds = 1000,
                         booster = "gbtree",
                         objective = "reg:linear",
                         nthread = 3,
                         max_depth = 25)

### Test model
xgb.test <- data.matrix(test.features)
test.data$log.SalePrice.xgb <- predict(model.xgboost, xgb.test)

test.data <- test.data %>% mutate(SalePrice.xgb = exp(log.SalePrice.xgb),
                                  residuals.xgb = SalePrice - SalePrice.xgb)

# Calculate RMSE
RMSE.xgb <- sqrt(mean(test.data$residuals.xgb ^ 2))
print(RMSE.xgb)

# xgb.plot.tree(feature_names = colnames(xgb.train), model = model.xgboost)







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Linear Model ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Train the linear model
lm.data <- data.frame(train.features, log.SalePrice = train.target)

model.lm <- lm(log.SalePrice ~ ., lm.data)

## Run Predictions
test.data$log.SalePrice.LM <- predict(model.lm, test.data)

## Evaluate
test.data <- test.data %>% mutate(SalePrice.LM = exp(log.SalePrice.LM),
                                  residuals.LM = SalePrice - SalePrice.LM)

## Calculate RMSE
RMSE.LM <- sqrt(mean(test.data$residuals.LM ^ 2))
print(RMSE.LM)






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Parent-level stacked learner ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

parent.train.data <- data.frame(log.SalePrice.RF = predict(model.rf, train.features),
                                log.SalePrice.xgb = predict(model.xgboost, xgb.train),
                                log.SalePrice.LM = predict(model.lm, train.features))


#### Try with RF ####
## Train stacked learner based on lower-level model outputs
stacked.model <- randomForest(x = parent.train.data, y = train.target,
                              importance = TRUE,
                              ntree = 50)

## Run predictions
test.data$log.SalePrice.stack.RF <- predict(stacked.model, test.data)





## Evaluate
test.data <- test.data %>% mutate(SalePrice.stack.RF = exp(log.SalePrice.stack.RF),
                                  residuals.stack.RF = SalePrice - SalePrice.stack.RF)

# Calculate RMSE
RMSE.stack.parent <- sqrt(mean(test.data$residuals.stack.RF ^ 2))
print(RMSE.stack.parent)







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Average of predictions ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test.data <- test.data %>% mutate(log.SalePrice.mean.pred = (log.SalePrice.RF + log.SalePrice.xgb + log.SalePrice.LM) / 3,
                                  SalePrice.mean.pred = exp(log.SalePrice.mean.pred),
                                  residuals.mean.pred = SalePrice - SalePrice.mean.pred)


# Calculate RMSE
RMSE.mean.pred <- sqrt(mean(test.data$residuals.mean.pred ^ 2))
print(RMSE.mean.pred)








#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Generate Kaggle Submission ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Import and tidy ####
# Import test dataset
competition.data <- read.csv("./Data/Raw/test.csv")

# Tidy the data
source("./Code/Modeling/Data_Cleanser.R")
competition.data <- clean_dataset(competition.data)

# Process factor levels (re-bin small levels)
source("./Code/Modeling/Data_Processor.R")
competition.data <- align_factors_with_model(model = model.rf, new.data = competition.data)




#### Create feature matrix ####
comp.features <- competition.data[, feature.list]

# Apply feature engineering operations
comp.features <- feature_engineering(feature.matrix = comp.features,
                                     neighborhood.ref.data = my.data,
                                     tax.dataset = tax.data,
                                     neighborhood.tax.betas = neighborhood.tax.betas)

# Add Id back in
comp.features$Id <- competition.data$Id





#### Check competition data to ensure all features are present and correct ####
# Generate schema for the training set
source("./Code/Modeling/Feature_Engineering_Functions.R")
model.schema <- capture_schema(features)

# Check that all model features are in place
model.check <- check_schema(new.data = comp.features, model.schema = model.schema)

# Last check, look for NAs in competition data
data.types <- sapply(comp.features, class)
na.investigate <- apply(comp.features, 2, function(x) length(which(is.na(x))))
data.summary.table <- data.frame(type = data.types, na.count = na.investigate)
data.summary.table$in.model <- ifelse(row.names(data.summary.table) %in% names(model.rf$forest$xlevels), TRUE, FALSE)









#### Run the predictions ####

# Individual Predictions
comp.features$log.SalePrice.RF <- predict(model.rf, comp.features)
comp.features$log.SalePrice.xgb <- predict(model.xgboost, data.matrix(comp.features))
comp.features$log.SalePrice.LM <- predict(model.lm, comp.features)

# Ensembles
comp.features$log.SalePrice.stack.RF <- predict(stacked.model, comp.features)
comp.features <- comp.features %>% mutate(log.SalePrice.mean.pred = (log.SalePrice.RF + log.SalePrice.xgb + log.SalePrice.LM) / 3)


#### Format for submission ####
comp.features <- comp.features %>% mutate(SalePrice = exp(comp.features$log.SalePrice.mean.pred)) %>%
  select(Id, SalePrice)



## Save the results
write.csv(comp.features, file = "./Data/Submissions/Itr7_ImprovedTaxValuation_Submission_2.csv",
          row.names = FALSE)




