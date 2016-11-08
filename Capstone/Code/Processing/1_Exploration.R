#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Re-format features to ordinal scale?:
# ExterQual, ExterCond, BsmtQual, BsmtCond, BsmtExposure, BsmtFinType1, HeatingQC,
# KitchenQual, Functional, FireplaceQu
#
# Can we pull in additional neighborhood data- i.e. school scores, walkability...
#
# Convert months to seasons
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



library(ggplot2)
library(dplyr)
source("./Code/Processing/Processing_Functions.R")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Load data for exploration ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

my.data <- read.csv("./Data/Raw/train.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Explore variables ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Inspect target variable in depth ####
plot_distribution(my.data, my.data$SalePrice, "SalePrice")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Clean Missing Values ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Manually set column typing on specific columns:
factor.columns <- c("MSSubClass")
my.data[, factor.columns] <- sapply(my.data[, factor.columns], as.factor)


#### Sort parameters by type ####
data.types <- sapply(my.data, class)
int.columns <- which(data.types != "factor")
cat.columns <- which(data.types == "factor")

# Investigate missing data
na.investigate <- apply(my.data, 2, function(x) length(which(is.na(x))))

# Build summary table
data.summary.table <- data.frame(type = data.types, factor.levels = NA, na.count = na.investigate)
data.summary.table$factor.levels <- sapply(my.data, function(x) length(levels(x)))


# Re-label NA records
amend.cols <- c("PoolQC", "MiscFeature", "Alley", "Fence", "FireplaceQu",
                "GarageFinish", "GarageQual", "GarageCond", "GarageType",
                "BsmtExposure", "BsmtFinType1", "BsmtFinType2", "BsmtQual",
                "BsmtCond", "MasVnrType")
my.data[,amend.cols] <- sapply(my.data[,amend.cols], amend_na)

# Impute missing values
my.data$LotFrontage[is.na(my.data$LotFrontage)] <- mean(my.data$LotFrontage, na.rm = TRUE)
my.data$Electrical[is.na(my.data$Electrical)] <- "SBrkr"  # This is the most common electrical type
my.data$MasVnrArea[is.na(my.data$MasVnrArea)] <- 0

# Drop features
my.data <- my.data %>% select(-GarageYrBlt)


# Re-set classes
data.types <- sapply(my.data, class)
char.columns <- colnames(my.data[,which(data.types == "character")])
my.data[, char.columns] <- lapply(my.data[, char.columns], as.factor)

data.types <- sapply(my.data, class)
int.columns <- which(data.types != "factor")
cat.columns <- which(data.types == "factor")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Relationship Plots ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Relationships with numerics
cor.result <- cor(my.data[, int.columns])
corrplot::corrplot(cor.result, method = "square")


## Relationships with categoricals
sale.price.data <- cbind(my.data[, cat.columns], SalePrice = my.data$SalePrice)
sale.price.lm <- lm(SalePrice ~ ., sale.price.data)
eta.sq <- lsr::etaSquared(sale.price.lm)
eta.sq <- data.frame(Variable = rownames(eta.sq), eta.sq)
eta.sq <- eta.sq %>% arrange(desc(eta.sq))

eta.sq.plot <- ggplot(eta.sq, aes(reorder(Variable, eta.sq), eta.sq)) +
  geom_bar(stat = "identity") +
  coord_flip()
eta.sq.plot
