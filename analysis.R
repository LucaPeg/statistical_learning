# libraries ---------------------------------------------------------------

library(tidyverse)
library(readr)
library(ggplot2)
library(GGally)
library(estimatr)
library(leaps)
library(glmnet)
library(dplyr)
library(tidymodels)
library(stats)
library(tree)
library(maxLik)
library(Matrix)
library(caret)
library(car)
library(ggrepel)
library(here)


# functions ---------------------------------------------------------------

# rmse function
rmse = function(predictions, data, y) {
  residuals = (predictions - data[[y]])
  mse = (1/nrow(data)) * sum((residuals^2))
  rmse = sqrt(mse)
  return(rmse)
}

# import data -------------------------------------------------------------
file <- here("data","full_dataset.csv")
data <-  read.csv(file, sep=",")

# data_an <- data we'll use in most of the analysis
data_an <-  data |> 
  filter(!is.na(sigi))|> # we omit the rows without sigi value
  select(-c(country, fos, sigid, gdp, pop, relOther, rel))

# descriptives ------------------------------------------------------------


# What is the relationship between sigi and democracy?
data_an |> # Apparently democracies are on average less discriminatory
  ggplot(aes(x = dem, y = sigi)) +
  geom_point(aes(color = region)) +
  geom_smooth(method = lm) 

#general summary
summary(data)

# correlations between variables

data |>
  select(cpi, dem, fragility, lgdp, gini, lifeexp, oilexp, lpop, sigi, urb) |>
  ggpairs()

data |>
  select(cpi, dem, fragility, lgdp, gini, lifeexp, oilexp, lpop, sigi, urb) |>
  ggcorr()

# Boxplots
data %>%
  select(cpi, fragility, lgdp, gini, lifeexp, oilexp, lpop, sigi, urb) %>%
  tidyr::pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Variable, y = Value)) +
  geom_boxplot() +
  labs(x = "Variable", y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Variable, scales = "free", nrow = 3)

# check NAs -> when we focus on SIGI we have few NA in other attributes
filtered_data <- data %>%
  filter(!is.na(sigi))

na_counts <- filtered_data %>% # get number of NAs for each attribute
  summarise(across(everything(), ~ sum(is.na(.)))) 

print(na_counts)

# densities for cpi
filtered_data |>
  filter(!is.na(fos)) |> # belieze, somalia, south sudan miss some values
  ggplot(aes(x = cpi, fill = fos)) +
  geom_density(alpha = 0.5)

# densities for all variables, grouped by fos
filtered_data %>%
  filter(!is.na(fos)) %>%
  pivot_longer(cols = c(cpi, fragility, lgdp, gini, lifeexp, lpop, sigi, urb),
               names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Value, fill = fos)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Variable, scales = "free", nrow = 3, ncol = 3) +
  guides(fill = guide_legend(title = "Groups", label.position = "right")) +
  theme(
    legend.position = c(0.85, 0.1),
    legend.direction = "vertical",
    legend.justification = "right",
    legend.box.just = "right",
    legend.title.align = 0.5,
    plot.margin = unit(c(1, 1, 1, 1), "lines"),
    legend.text = element_text(size = 12),  
    legend.title = element_text(size = 14),  
    legend.key.size = unit(1.5, "lines")
  )


# how is the SIGI variable distributed?
data |>
  ggplot(aes(x = sigi)) +
  geom_histogram(bins = 50)

data |> 
  ggplot(aes(y = sigi)) +
  geom_boxplot() # median is around 26; IQR between 17 and 41

# Find appropriate threshold for classification problem:
filtered_data |>
  filter(sigi > 35) |> # maybe 35 could make sense? 
  nrow()               


# split train and test ----------------------------------------------------

set.seed(42)

# create an index for the train/test division
train_index = sample(c(TRUE,FALSE), nrow(data_an),rep=TRUE, prob = c(0.8,0.2))
data_an <- cbind(train_index,data_an)
table(data_an$train)

# create train and test data
train <- data_an |> filter(train_index == TRUE)
test <- data_an |> filter(train_index == FALSE)
train = train[, !colnames(train) %in% "train_index"]
test = test[, !colnames(test) %in% "train_index"]

train <-  na.omit(train) # there are few NAs that prevent predicting
test <-  na.omit(test)   # get rid of NAs

dim(train)
dim(test)


# select best model for linear reg --------------------------------------
regfit.full=regsubsets(sigi~.,data=train[, !names(train) %in% c("code", "region")]) 
reg.summary <- summary(regfit.full) 
reg.summary
par(mfrow=c(2,2)) # set up plot layout
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
max_adjr2 <- which.max(reg.summary$adjr2)
points(max_adjr2,reg.summary$adjr2[max_adjr2], col="red",cex=2,pch=20)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
min_cp <- which.min(reg.summary$cp)
points(min_cp,reg.summary$cp[min_cp],col="red",cex=2,pch=20)
min_bic <- which.min(reg.summary$bic)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(min_bic,reg.summary$bic[min_bic],col="red",cex=2,pch=20)

coef(regfit.full, which.max(reg.summary$adjr2)) # best according to adjr2
coef(regfit.full, which.max(reg.summary$bic)) # best bic, actually is relMuslim
# plot variables selection
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")  # 6 vars
plot(regfit.full,scale="Cp")     # 4 vars
plot(regfit.full,scale="bic")    # 2 vars
coef(regfit.full,6) # best according to AdjR2 

#issue : reg.summary contains relMuslim, but the plots don't show it.

test.mat=model.matrix(sigi~.,data=test)
val.errors=rep(NA,6)


# best subset model -------------------------------------------------------

# Best model according to BIC
ols_minimal = lm(sigi~fragility+relMuslim, data = train) # lm_robust is the same
summary(ols_minimal)
# Compute the VIF of the model
vif_minimal= vif(lm(sigi~fragility + relMuslim, data = train))
vif_minimal #very low, unlike with best adjr2 model

# Best model according to AdjR2
best.adjr2 <-  lm(sigi ~ cpi+opec+gini+urb+relMuslim+fragility,data = train)
summary(best.adjr2) # just 0.02 improvement on Adjr2 with 4 additional vars

# The VIF is pretty high for many variables (cpi & fragility)
vif_best.adjr2 <- vif(lm(sigi ~ cpi+ opec +fragility + gini + urb + relMuslim, 
                                data = train))
vif_best.adjr2

# Prediction results with "big" model
pred.best.adjr2 <- predict(best.adjr2, newdata = test)
rmse(fitted(best.adjr2), train, "sigi") #training error
rmse(pred.best.adjr2, test, "sigi") #test error

# Prediction results with "minimal" model
pred.minimal <- predict(ols_minimal, newdata = test) 
rmse(fitted(ols_minimal), train, "sigi") # training error is slightly greater
rmse(pred.minimal, test,"sigi") # test error is same as big model


# Ridge -------------------------------------------------------------------


# Create model matrices for the training and testing data

x <- model.matrix(sigi ~ cpi + dem + opec + fragility + gini + lifeexp + oilexp +
                    urb + lgdp + lpop + relChristian + relMuslim,
                  data = train)

x.test <- model.matrix(sigi ~ cpi + dem + opec + fragility + gini + lifeexp + oilexp +
                         urb + lgdp + lpop + relChristian + relMuslim,
                       data = test)

y <- train$sigi
y.test <- test$sigi

# Initialize a grid for lambda
grid = 10^seq(10, -2, length = 100)

# Fit Ridge regression model
ridge.mod = glmnet(x, y, alpha = 0, lambda = grid, thresh = 1e-12)

# CV to find optimal lambda
cv.out = cv.glmnet(x, y, alpha = 0)
bestlam = cv.out$lambda.min

# Predict using the best lambda obtained from cross-validation
ridge.train = predict(ridge.mod, s = bestlam, newx = x)
ridge.pred = predict(ridge.mod, s = bestlam, newx = x.test)

# RMSE Ridge
rmse(ridge.train, train, 'sigi')
rmse.ridge = rmse(ridge.pred, test, "sigi")
print(rmse.ridge) # test error is slightly better than the minimal model

# Coefficients at with best lambda
print(coef(ridge.mod, s = bestlam))



# Lasso -------------------------------------------------------------------

# Fit Lasso regression model
lasso.mod = glmnet(x, y, alpha = 1, lambda = grid)

# CV to find optimal lambda for Lasso
cv.out = cv.glmnet(x, y, alpha = 1)
bestlam = cv.out$lambda.min

# Predict using the best lambda obtained from cross-validation for Lasso
lasso.train = predict(lasso.mod, s = bestlam, newx = x)
lasso.pred = predict(lasso.mod, s = bestlam, newx = x.test)

# RMSE Lasso
rmse(lasso.train, train,'sigi') # training error
rmse_lasso = rmse(lasso.pred, test, "sigi")
print(rmse_lasso) # test error

# Print coefficients at best lambda for Lasso
print(coef(lasso.mod, s = bestlam))


# diagnostics -------------------------------------------------------------
par(mfrow=c(1,1))

###### SCATTER OF COOK'S AND LEVERAGE 

# We focus on the minimal model (the one that minimized BIC)
# Calculate leverage and Cook's distance
leverage <- hatvalues(ols_minimal)
cooks_d <- cooks.distance(ols_minimal)

# Define thresholds
cooks_threshold <- 4 / (length(cooks_d) - 2)  # Common threshold for Cook's distance
leverage_threshold <- 2 * mean(leverage)      # Common threshold for leverage

# Identify observations above either threshold
high_influence <- (cooks_d > cooks_threshold) | (leverage > leverage_threshold)


# Final plot
ggplot(train, aes(x = leverage, y = cooks_d)) +
  geom_point(color = ifelse(high_influence, "red", "black")) +  # Points
  geom_text_repel(aes(label = ifelse(high_influence, code, "")),  # Only label high influence points
                  box.padding = 0.35, point.padding = 0.5,
                  segment.color = 'grey50') +  # Adds lines to text for clarity
  geom_hline(yintercept = cooks_threshold, color = "red", linetype = "dashed") +
  geom_vline(xintercept = leverage_threshold, color = "blue", linetype = "dashed") +
  labs(x = "Leverage (Hat values)", y = "Cook's Distance", title = "Cook's Distance vs. Leverage with Labels") +
  theme_minimal()

# Standardized residuals and QQ plot
std_residuals_lm <- rstandard(ols_minimal)
plot(train$sigi, std_residuals_lm, xlab = "SIGI", ylab = "Standardized Residuals",
     main = "Standardized Residuals vs. SIGI (LM)")
abline(h = 0, col = "grey")
lines(lowess(train$sigi, std_residuals_lm), col = "blue")
abline(h = 3, col = "red", )
abline(h = -3, col = 'red')

# QQ plot
qqnorm(std_residuals_lm, main = "Q-Q Plot of Standardized Residuals (LM)")
qqline(std_residuals_lm, col = "red")

# unsupervised models -----------------------------------------------------
# PCA, then K-means 
# PCA -> I use the dataset without train/test split #####

# divide categorical and numerical data for scaling
# Categorical attributes
data_an <- na.omit(data_an)
data_an <- data_an[data_an$code != 'SAU', ] # SAU was outlier in PCA

categorical_data <- data_an[, c("code", "region" )]
dummies <- data_an[,c("relChristian","relMuslim","opec")]
# Numeric attributes
numeric_data <- data_an[, !(names(data_an) # we drop sigi as well
                            %in% c("code","region","relChristian",
                                   "relMuslim", "opec", "X", "sigi"))] 

# normalize numeric data
scaled_numeric_data <- scale(numeric_data)
#merge all numerical variables
scaled_numeric_data <- cbind(scaled_numeric_data, dummies)
# merge the dataset back together
final_data <- cbind(categorical_data, scaled_numeric_data)

# Perform PCA
pca_result <- prcomp(scaled_numeric_data)
# Create a dataframe of the PCA scores
pca_scores <- as.data.frame(pca_result$x)
# Add back the categorical data for plotting
plot_data <- cbind(pca_scores, categorical_data)

# Plotting the first two principal components with ggplot2
ggplot(plot_data, aes(x = PC1, y = PC2, color = region)) +
  geom_point(alpha = 0.5) +
  labs(title = "PCA of Dataset", x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal()

# Scree plot
plot(pca_result$sdev^2, type = 'b', main = "Scree Plot", xlab = "Principal Component", ylab = "Eigenvalues")
# Biplot of the first two PCs
biplot(pca_result)

# K-MEANS ####
# Using the first two principal components
pc_data <- pca_result$x[, 1:2]

# K-means clustering
set.seed(123)  # for reproducibility
kmeans_result <- kmeans(pc_data, centers = 2)  # two clusters

# Plotting K-means results

## merge pc_data with categorical in order to spot the outlier
pc_data <- cbind(pc_data, categorical_data)

# # plot PCA
plot(pc_data[,1], pc_data[,2], col = kmeans_result$cluster, pch = 20, main = "K-means on Principal Components")
points(kmeans_result$centers[, 1], kmeans_result$centers[, 2], col = 1:3, pch = 8, cex = 2)

# Subsetting data where PC2 > 5
pc_data_high_pc2 <- pc_data[pc_data[, 2] > 5, ]
if (nrow(pc_data_high_pc2) > 0) {
  text(pc_data_high_pc2[, 1], pc_data_high_pc2[, 2], 
       labels = pc_data_high_pc2$code, pos = 1, cex = 0.8, col = 'black')
}



# Classification ----------------------------------------------------------

# Change sigi into a dummy variable according to a threshold.
# Use logistic regression and knn to classify datapoints
# Makes less theoretical sense due to sigi's construction
# But it is a clear exercise
