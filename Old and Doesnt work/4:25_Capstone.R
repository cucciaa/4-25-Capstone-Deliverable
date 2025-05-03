title: "Exploratory Data Analysis with R- Capstone Deliverable"
author: "Anthony Cuccia"
date: "4/25/2025"
output: pdf_document
---

# 1. Load or install required packages
required_packages <- c("tidyverse", "broom", "corrplot")
installed <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!pkg %in% installed) install.packages(pkg)
}
library(tidyverse)
library(broom)
library(corrplot)

# 2. Data Loading: read directly from UCI repository
red_url   <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
white_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"

red  <- read.csv(red_url, sep = ";")
white <- read.csv(white_url, sep = ";")

# Add a type indicator
red$type   <- "red"
white$type <- "white"

# Combine datasets
wine <- bind_rows(red, white)

# 3. Data Understanding
# Dimensions and structure
cat("Red wine samples:", nrow(red), "with", ncol(red), "features\n")
cat("White wine samples:", nrow(white), "with", ncol(white), "features\n")
cat("Combined dataset:", nrow(wine), "samples and", ncol(wine), "columns\n")

# Preview
glimpse(wine)
summary(wine)

# 4. Data Preparation
# Check missing values
na_counts <- colSums(is.na(wine))
print(na_counts)
# No missing values in this dataset

# 5. Exploratory Data Analysis
# 5.1 Distribution of Quality by Wine Type
ggplot(wine, aes(x = as.factor(quality), fill = type)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Wine Quality by Type",
       x = "Quality Score", y = "Count") +
  theme_minimal()

# 5.2 Boxplots of Key Chemical Properties by Type
features_to_plot <- c("alcohol", "pH", "residual.sugar", "volatile.acidity")
for (feature in features_to_plot) {
  p <- ggplot(wine, aes_string(x = "type", y = feature)) +
    geom_boxplot() +
    labs(title = paste(feature, "by Wine Type"), y = feature) +
    theme_minimal()
  print(p)
}

# 5.3 Correlation Matrices for Red and White Wines
numeric_vars <- select(wine, -type)
cor_red   <- cor(select(red, -type))
cor_white <- cor(select(white, -type))

# Plot correlation heatmaps
corrplot(cor_red,   method = "color", title = "Red Wine Correlation",   mar = c(0,0,1,0))
corrplot(cor_white, method = "color", title = "White Wine Correlation", mar = c(0,0,1,0))

# 6. Statistical Testing
# 6.1 Compare mean quality between red and white wines
test_quality <- t.test(quality ~ type, data = wine)
print(tidy(test_quality))

# 6.2 Correlation test: alcohol vs quality by type
test_red_alcohol   <- cor.test(red$alcohol,   red$quality)
test_white_alcohol <- cor.test(white$alcohol, white$quality)
cat("Red wine: correlation between alcohol and quality", round(test_red_alcohol$estimate, 3),
    "p-value =", round(test_red_alcohol$p.value, 4), "\n")
cat("White wine: correlation between alcohol and quality", round(test_white_alcohol$estimate, 3),
    "p-value =", round(test_white_alcohol$p.value, 4), "\n")

# 7. Actionable Insights
# - White wines tend to have higher average quality scores than red wines (see t-test).
# - Alcohol content is positively correlated with quality in both types, stronger for red wines.
# - Volatile acidity negatively impacts quality; consider controlling fermentation to reduce it.
# - Fixed and residual sugar show weaker associations, but may differ by type.


