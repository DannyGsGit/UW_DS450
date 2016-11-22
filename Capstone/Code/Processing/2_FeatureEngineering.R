#### Feature Engineering ####

library(dplyr)
library(ggplot2)
library(tidyr)

#~~~~~~~~~~~~~~~~~~~~~~~
#### Import dataset ####
#~~~~~~~~~~~~~~~~~~~~~~~

load("./Data/Processed/1_Post_Cleansing.RData")



#~~~~~~~~~~~~~~~~~~~~~
#### Drop Columns ####
#~~~~~~~~~~~~~~~~~~~~~

my.data <- my.data %>% select(-Street, -Utilities, -PavedDrive, - GarageArea,
                              -TotalBsmtSF, -Condition2, -RoofMatl, -PoolQC)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Summary Plots of Categorical Features ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lattice::histogram(my.data$Neighborhood)  # Could benefit from binning
lattice::histogram(my.data$KitchenQual)
lattice::histogram(my.data$BsmtQual)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Feature engineering ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### 0- Add log-price column ####
my.data <- my.data %>% mutate(log.sale.price = log(SalePrice))





#### 1- Neighborhood processing ####
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

# Drop original column
my.data$Neighborhood <- NULL








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

# Drop Condition1
my.data$Condition1 <- NULL





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

# Drop original
my.data$MSSubClass <- NULL







#### 4- Alley ####
my.data <- my.data %>% mutate(alley.binned = ifelse(Alley == "None", 0, 1)) %>% select(-Alley)







