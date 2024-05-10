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
library(ggrepel) # avoid overlapping on plots
library(here)
library(ggpubr) # plots
library(patchwork) # plots
library(MASS) 
library(lmtest) # homoskedasticity
library(rpart) # trees
library(randomForest)
library(lattice)
library(cluster)
library(stargazer) # export to LaTeX

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
  dplyr::select(-c(country, fos, sigid, gdp, pop, relOther, rel)) # MASS masks it

# descriptives ------------------------------------------------------------

# Distrigbution of SIGI: should we use log(sigi)?
data$lsigi <- log(data$sigi)

# how is the SIGI variable distributed? 
data |> 
  ggplot(aes(y = sigi)) +
  geom_boxplot() # median is around 26; IQR between 17 and 41

# Normality test for sigi and lsigi
shapiro.test(data$sigi) # not normal
shapiro.test(data$lsigi) # still not normal

# Visualization of density with normal distribution overlay
ggdensity(data, x = "sigi", fill = "lightgray", 
          title = "Distribution of SIGI with overlay of normal distirbution") +
  stat_overlay_normal_density(color = "red", linetype = "dashed") # skewed data

ggdensity(data, x = "lsigi", fill = "lightgray",
          title = "ln(sigi) and normal distribution") + # less skewed but still not normal
  stat_overlay_normal_density(color = "red", linetype = "dashed") 

# qqplot sigi
qqnorm(data$sigi, main = "Q-Q Plot of SIGI")
qqline(data$sigi, col = "red")

# qqplot lsigi
qqnorm(data$lsigi, main = "Q-Q Plot of ln(SIGI)") # this looks better
qqline(data$lsigi, col = "red")

# What is the relationship between sigi and democracy?
data |> # Apparently democracies are on average less discriminatory
  ggplot(aes(x = dem, y = sigi)) +
  geom_point(aes(color = region)) +
  geom_smooth(method = lm) 

data |> # Apparently democracies are on average less discriminatory
  ggplot(aes(x = dem, y = lsigi)) +
  geom_point(aes(color = region)) +
  geom_smooth(method = lm) 

#general summary
summary(data)

# correlations between variables

data |>
  dplyr::select(cpi, dem, fragility, lgdp, gini, lifeexp, oilexp, lpop, sigi, urb) |>
  ggpairs()

data |>
  dplyr::select(cpi, dem, fragility, lgdp, gini, lifeexp, oilexp, lpop, sigi, urb) |>
  ggcorr()

# Boxplots
data %>%
  dplyr::select(cpi, fragility, lgdp, gini, lifeexp, oilexp, lpop, sigi, urb) %>%
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



# What if we use lsigi? ---------------------------------------------------
# data_an$sigi <-  log(data_an$sigi) # First do analysis without, then try 
# all the analysis with lsigi

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
plot(regfit.full,scale="adjr2")  # 6 vars sigi # 5 vars lsigi (-urb)
plot(regfit.full,scale="Cp")     # 4 vars
plot(regfit.full,scale="bic")    # 2 vars sigi # 3 vars lsigi (+gini)
coef(regfit.full,6) # best according to AdjR2 

#issue : reg.summary contains relMuslim, but the plots don't show it.
test.mat=model.matrix(sigi~.,data=test)
val.errors=rep(NA,6)


# best subset model -------------------------------------------------------

# Best model according to BIC
ols_minimal = lm(sigi~fragility+relMuslim, data = train) # lm_robust is the same
summary(ols_minimal)# if lsigi BIC takes "gini" as well

summary(rr.minimal <- rlm(sigi ~ fragility + relMuslim, data = train)) # robust

stargazer(ols_minimal, type = "latex") # export table
# Compute the VIF of the model
vif_minimal= vif(lm(sigi~fragility + relMuslim, data = train))
vif_minimal #very low, unlike with best adjr2 model

# Best model according to AdjR2
best.adjr2 <-  lm(sigi ~ cpi+opec+gini+urb+relMuslim+fragility,data = train)
summary(best.adjr2) # just 0.02 improvement on Adjr2 with 4 additional vars

stargazer(best.adjr2, type = "latex") 
# The VIF is pretty high for many variables (cpi & fragility)
vif_best.adjr2 <- vif(lm(sigi ~ cpi+ opec +fragility + gini + relMuslim + urb, # - urb if lsigi
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


# OLS diagnostics ----------------------------------------------------

# Fitted against residuals
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(ols_minimal, las = 1)

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

# reset layout
par(mfrow=c(1,1))

# Final plot
ggplot(train, aes(x = leverage, y = cooks_d)) +
  geom_point(color = ifelse(high_influence, "red", "black")) +  # Points
  geom_text_repel(aes(label = ifelse(high_influence, code, "")),  # Only label high influence points
                  box.padding = 0.35, point.padding = 0.5,
                  segment.color = 'grey50') +  # Adds lines to text for clarity
  geom_hline(yintercept = cooks_threshold, color = "red", linetype = "dashed") +
  geom_vline(xintercept = leverage_threshold, color = "blue", linetype = "dashed") +
  labs(x = "Leverage (Hat values)", y = "Cook's Distance", title = "Cook's Distance vs. Leverage with Labels") +
  theme_minimal() # QAT is outlier, MYS, BFA, CMR and SGP have high cd


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

# are residuals normal?
shapiro.test(std_residuals_lm) # yes they are

# are residuals homoskedastic?

# For the minimal model
bptest(ols_minimal) # we reject H0 -> heteroskedasticity -> we should use robust reg

# For the best adjusted R^2 model
bptest(best.adjr2) # we cannot reject H0 -> homoskedasticity


# OLS without outliers ----------------------------------------------------

# List of outlier country codes
outlier_codes <- c("QAT","BFA", "CMR", "SGP", "MYS" )

# Create a new dataset excluding these outliers
data_noout <- train[!train$code %in% outlier_codes, ]

ols_minimal_out = lm(sigi~fragility+relMuslim, data = data_noout)
ols_full_out =lm(sigi ~ cpi+opec+gini+urb+relMuslim+fragility,data = data_noout)
summary(ols_minimal_out)
summary(ols_full_out)

# Prediction results with "big" model
pred.ols_full_out <- predict(ols_full_out, newdata = test)
rmse(fitted(ols_full_out), data_noout, "sigi") #training error
rmse(pred.ols_full_out, test, "sigi") #test error

# Prediction results with "minimal" model

pred.minimal_noout <- predict(ols_minimal_out, newdata = test) 
rmse(fitted(ols_minimal_out), data_noout, "sigi") # training error is slightly greater
rmse(pred.minimal_noout, test,"sigi")

# While train error reduces, test error does not

# Robust minimal OLS ------------------------------------------------------

rr.minimal <- rlm(sigi ~ fragility + relMuslim, data = train) # robust
pred.rr.minimal <- predict(rr.minimal, newdata = test) 
rmse(fitted(rr.minimal), train, "sigi") # training error is slightly greater
rmse(pred.rr.minimal, test,"sigi") # 12 instead of 11, doesn't change much


# OLS Test diagnostics --------------------------------------------------------
# Predictions for minimal model
pred.minimal <- predict(ols_minimal, newdata = test)

# Residuals for minimal model
residuals_minimal <- test$sigi - pred.minimal

# Predictions for best adjusted R^2 model
pred.best.adjr2 <- predict(best.adjr2, newdata = test)

# Residuals for best adjusted R^2 model
residuals_best.adjr2 <- test$sigi - pred.best.adjr2


# Plot residuals vs. predicted (minimal model)
par(mfrow=c(1,1))

plot(pred.minimal, residuals_minimal, main="Residuals vs. Predicted (Minimal Model)",
     xlab="Predicted", ylab="Residuals")
abline(h=0, col="red")

# Plot residuals vs. predicted (best adjR2 model)
plot(pred.best.adjr2, residuals_best.adjr2, main="Residuals vs. Predicted (Best AdjR2 Model)",
     xlab="Predicted", ylab="Residuals")
abline(h=0, col="red")

# Test for homoskedasticity
ncvTest(lm(residuals_minimal ~ pred.minimal))
ncvTest(lm(residuals_best.adjr2 ~ pred.best.adjr2))

# QQ plot for minimal model residuals
qqnorm(residuals_minimal, main="QQ Plot of Residuals (Minimal Model)")
qqline(residuals_minimal, col="red")

# QQ plot for best adjR2 model residuals
qqnorm(residuals_best.adjr2, main="QQ Plot of Residuals (Best AdjR2 Model)")
qqline(residuals_best.adjr2, col="red")


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



# Trees -------------------------------------------------------------------
tree_model <- rpart(sigi ~ ., data = train[, !names(train) %in% c("code", "region")], 
                    method = "anova")
# Plot the tree
plot(tree_model, uniform = TRUE, main = "Decision Tree for SIGI")
text(tree_model, use.n = TRUE)

# Prune the tree
pruned_tree <- prune(tree_model, cp = tree_model$cptable[which.min(tree_model$cptable[, "xerror"]), "CP"])

# Plot the pruned tree
plot(pruned_tree, uniform = TRUE, main = "Pruned Decision Tree for SIGI")
text(pruned_tree, use.n = TRUE)

# Predictions on training data
train_pred_tree <- predict(tree_model, train, type = "vector")
train_pred_pruned <- predict(pruned_tree, train, type = "vector")
# Predictions on test data
test_pred_tree <- predict(tree_model, test, type = "vector")
test_pred_pruned <- predict(pruned_tree, test, type = "vector")

# Calculate RMSE for training and test data
rmse(train_pred_tree, train, "sigi")  # training full tree
rmse(test_pred_tree, test, 'sigi')    # test full tree

rmse(train_pred_pruned, train, "sigi")
rmse(test_pred_pruned, test, "sigi") # pruned performs worse

# Try CV to get a better Tree

set.seed(42)  # same as before
train_control <- trainControl(
  method = "cv",        # Cross-validation
  number = 10           # Number of folds
)

# Range of complexity parameter values
cp_grid <- expand.grid(cp = seq(0.001, 0.1, by = 0.002))

# Train the model
tree_cv <- train(
  sigi ~ ., 
  data = train[, !names(train) %in% c("code", "region")],
  method = "rpart",
  trControl = train_control,
  tuneGrid = cp_grid
)

# Print the results
print(tree_cv)
plot(tree_cv)

# Best model parameters
tree_cv$bestTune

# Additional model details
summary(tree_cv$finalModel)

train_tree_cv <-  predict(tree_cv$finalModel, train, type = "vector")
rmse(train_tree_cv, train, "sigi") # train error

test_tree_cv <- predict(tree_cv$finalModel, test, type = 'vector')
rmse(test_tree_cv, test, "sigi") # test error ~ worse


# Random Forest -----------------------------------------------------------


set.seed(42) # same as before

# Define training control
train_control <- trainControl(
  method = "cv",                  # Cross-validation
  number = 10,                    # Number of folds
  savePredictions = "final",      # Save predictions for each fold
  summaryFunction = defaultSummary  # Default summary uses RMSE, Rsquared, and MAE
)

# Exclude non-predictive columns and train the model
model_rf <- train(
  sigi ~ ., 
  data = train[, !names(train) %in% c("code", "region")],
  method = "rf",
  trControl = train_control,
  metric = "RMSE"
)

# Print the results to show the RMSE from cross-validation
print(model_rf$results) # mtry is the number of variables randomly selected as 
                        # candidates at each split
                        # MAE gives a simpler interpretation of average error magnitude.

# Predict on the test data
predictions <- predict(model_rf, newdata = test)

# Calculate RMSE on test data
test_rmse <-  rmse(predictions, test, "sigi")

# Output RMSE for the test data
print(paste("Test RMSE: ", test_rmse))

# Plot actual vs predicted values for visual inspection
plot(test$sigi, predictions, main = "Actual vs. Predicted SIGI",
     xlab = "Actual SIGI", ylab = "Predicted SIGI")
abline(0, 1, col = "red")  # Line showing perfect predictions


# Unsupervised Models -----------------------------------------------------
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


# Hierarchical Clustering -------------------------------------------------

# Assuming data_an is the complete dataset

# Step 1: Filter out non-numeric columns and remove rows with NAs
data_for_clustering <- data_an[, sapply(data_an, is.numeric)]
data_for_clustering <- data_for_clustering |> 
  dplyr::select(-c(sigi, X))  # Excluding non-relevant numeric columns
data_for_clustering <- na.omit(data_for_clustering)

# Step 2: Standardize the data
data_scaled <- scale(data_for_clustering)

# Step 3: Compute the Euclidean distance matrix
dist_matrix <- dist(data_scaled, method = "euclidean")

# Step 4: Perform hierarchical clustering using the Ward's method
hc <- hclust(dist_matrix, method = "ward.D2")

# Step 5: Plot the dendrogram
plot(hc, labels = FALSE, hang = -1, main = "Hierarchical Clustering Dendrogram")

# Step 6: Cut the dendrogram to form clusters
k <- 4  # Number of clusters
clusters <- cutree(hc, k = k)

# Step 7: Track rows used in clustering
rows_with_data <- complete.cases(data_for_clustering)

# Step 8: Create a cluster column in the original dataset initialized with NA
data_an$cluster <- NA

# Step 9: Assign clusters only to rows without NA
data_an$cluster[rows_with_data] <- clusters

# Step 10: Summary statistics by cluster
# It's crucial to use the complete data_an with clusters for aggregation
summary_stats <- aggregate(data_an[, sapply(data_an, is.numeric)], 
                           by = list(cluster = data_an$cluster), 
                           FUN = mean)

# Print summary statistics
print(summary_stats)


# Plot the dendrogram
plot(hc, labels = FALSE, main = "Hierarchical Clustering Dendrogram")

# Retrieve the order of the leaves (indices of the original observations in the final order)
leaf_order <- hc$order

# Get the labels corresponding to the ordered leaves
labels <- data_an$code[leaf_order]

# Add labels at the bottom
text(x = 1:length(leaf_order),  # x-coordinates are simply 1 to the number of leaves
     y = par("usr")[3] - 0.5,  # slightly below the lower x-axis limit
     labels = labels, 
     srt = 45,  # angle of text rotation
     adj = 1,  # adjust text alignment
     xpd = NA,  # allow text to be placed outside plot region
     cex = 0.5)  # adjust text size to fit (decrease if still overlapping)

# Get countries for each cluster

# Create an empty list to store country codes for each cluster
cluster_codes <- vector("list", length = max(clusters))

# Iterate over each cluster
for (i in 1:max(clusters)) {
  # Get country codes for the current cluster
  cluster_codes[[i]] <- data_an$code[clusters == i]
}

# Display the country codes for each cluster
for (i in 1:length(cluster_codes)) {
  cat("Cluster", i, ": ", paste(cluster_codes[[i]], collapse = ", "), "\n")
}

# TODO: translate the codes to countries, comment on their summary statistics by cluster

