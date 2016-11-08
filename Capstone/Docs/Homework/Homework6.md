# Perform Data Analysis
> What were top 3-5 observations (describe w/ screenshots)?

* Distribution of sale prices
* The dramatic effect of neighborhood on home price
* Many apparently minor features showed outsized effect in the eta-squared analysis, likely due to class bias. It will be important to keep an eye on these features in modeling.








# What features appear most promising for modeling?
> Describe how features were selected, including screenshots if appropriate

Feature filtering was approached using a combination of correlation plots (for numerical features) and eta-squared (for categorical features).








# Data tidying
> Deal with missing values?
> Scale the attributes?

### Missing values
Initial evaluation of missing values produced the following table:

[IMAGE TABLE]

Re-Label as "None":
* PoolQC
* MiscFeature
* Alley
* Fence
* FireplaceQu
* GarageFinish
* GarageQual
* GarageCond
* GarageType
* BsmtExposure
* BsmtFinType2
* BsmtQual
* BsmtCond
* BsmtFinType1
* MasVnrArea
* MasVnrType

Impute Values:
* LotFrontage
* Electrical

Drop Features:
* GarageYrBlt


# Make initial modeling attempt to set baseline
