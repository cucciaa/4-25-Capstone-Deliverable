# Wine Quality Analysis Script

# This script performs a complete exploratory data analysis (EDA)
# comparing red and white wines from the UCI Wine Quality dataset.
# Each section includes code and comments explaining **what** is done,
# **why** it is done, and the **significance** of the findings.

# 1. Load and Install Required Packages
# Why: Ensure reproducible environment and avoid missing function errors.
required_packages <- c("tidyverse", "broom", "corrplot")
installed <- rownames(installed.packages())
to_install <- setdiff(required_packages, installed)
if (length(to_install) > 0) install.packages(to_install)

library(tidyverse) # Data manipulation and plotting
library(broom)     # Tidy statistical test outputs
library(corrplot)  # Correlation heatmaps

# 2. Data Loading and Merging
# Why: Combine red and white wine data for direct side-by-side comparison.
red_url   <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
white_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"

red   <- read.csv(red_url, sep = ";")
white <- read.csv(white_url, sep = ";")

red$type   <- "red"    # Label for wine type\ nwhite$type <- "white"
wine <- bind_rows(red, white)  # Merge into single dataset

# 3. Initial Data Exploration
# Why: Understand sample sizes, structure, and summary statistics.
cat("Red wine samples:", nrow(red), "| White wine samples:", nrow(white), "\n")
glimpse(wine)  # Data structure overview
summary(wine)  # Basic statistics for all variables

# 4. Missing Values Check
# Why: Ensure no missing data will bias the analysis.
na_counts <- colSums(is.na(wine))
print(na_counts)  # All zeros indicates complete data

# 5. Exploratory Data Analysis (EDA)
# Focus on: 1) Quality distribution, 2) Key chemical properties,
# 3) Inter-variable correlations.

# 5.1 Quality Distribution by Wine Type
# What: Bar chart of quality scores for each type
# Why: Quality is the primary outcome of interest
# Significance: Shows if one type generally rates higher

quality_plot <- ggplot(wine, aes(x = factor(quality), fill = type)) +
  geom_bar(position = "dodge") +
  labs(title = "Quality Score Distribution by Wine Type",
       x = "Quality Score (0-10)", y = "Count of Bottles") +
  theme_minimal()
print(quality_plot)

# 5.2 Boxplots of Key Chemical Properties
# What: Boxplots for alcohol, pH, residual sugar, volatile acidity
# Why: These drive taste, aroma, and perceived quality
# Significance: Highlights chemical differences explaining quality patterns

features <- c("alcohol", "pH", "residual.sugar", "volatile.acidity")
for (feat in features) {
  p <- ggplot(wine, aes(x = type, y = .data[[feat]])) +
    geom_boxplot() +
    labs(title = paste(feat, "by Wine Type"), y = feat) +
    theme_minimal()
  print(p)
}

# 5.3 Correlation Heatmaps
# What: Correlation matrices for red vs. white wines
# Why: Identify which properties co-vary with quality and each other
# Significance: Guides targeted quality improvements

cor_red   <- cor(select(red, -type))
cor_white <- cor(select(white, -type))
corrplot(cor_red,   method = "color", title = "Red Wine Correlations",   mar = c(0,0,1,0))
corrplot(cor_white, method = "color", title = "White Wine Correlations", mar = c(0,0,1,0))

# 6. Statistical Testing
# Confirm that observed differences are statistically significant.

# 6.1 Compare Mean Quality Between Types
# What: t-test of quality ~ type
# Why: Test if average ratings differ beyond chance
# Significance: A p-value < 0.05 indicates a true mean difference

test_quality <- t.test(quality ~ type, data = wine)
print(tidy(test_quality))

# 6.2 Alcohol vs. Quality Correlation by Type
# What: Pearson correlation tests
# Why: Quantify strength and significance of alcohol-quality link
# Significance: Helps assess alcohol as a lever for improving quality

test_red_alc   <- cor.test(red$alcohol,   red$quality)
test_white_alc <- cor.test(white$alcohol, white$quality)
cat("Red wine: r =", round(test_red_alc$estimate, 3), "p =", signif(test_red_alc$p.value, 3), "\n")
cat("White wine: r =", round(test_white_alc$estimate, 3), "p =", signif(test_white_alc$p.value, 3), "\n")

# 7. Actionable Insights
# - White wines statistically score higher: adapt white winemaking techniques for reds.
# - Alcohol correlates positively with quality: optimize fermentation to hit target alcohol.
# - Reds show higher acidity: consider blending or acid adjustment for smoother flavor.
# - Residual sugar varies: fine-tune sugar levels to match consumer taste preferences.

# End of analysis.
