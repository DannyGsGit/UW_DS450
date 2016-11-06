library(ggplot2)
library(reshape2)

#### Load data for exploration ####
my.data <- read.csv("./Data/Raw/train.csv")

#### Explore variables ####

# High-level data summary
str(my.data)

# Sort parameters by type
data.types <- sapply(my.data, class)
int.columns <- which(data.types == "integer")

## Normalize and plot integer params to inspect information content
# Calculate zscores
int.data.zscore <- scale(my.data[, int.columns], center = TRUE, scale = TRUE)

# Melt data
int.data.zscore <- melt(int.data.zscore)

# Plot
int.boxplots <- ggplot(int.data.zscore, aes(Var2, value)) +
  geom_boxplot() +
  coord_flip()

int.boxplots
