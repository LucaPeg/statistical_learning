
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


# import data -------------------------------------------------------------

data <-  read.csv("~/GitHub/sl_exam/statistical_learning/sigi_dataset.csv", sep=";")
# fix(data) to manually fix the data
data$sigid <- ifelse(data$sigi > 40, 1,0) # add sigi dummy for logistic / knn
data$lgdp <- log(data$gdp)
data$lpop <-  log(data$pop)


# create dummies from religion, then modify the code accordingly
sum(is.na(data$rel))
data$rel[is.na(data$rel)] <- "Missing"
# Assuming 'data' is your dataset and 'cat_var' is your categorical variable
rel_dummies <- model.matrix(~ rel - 1, data = data, na.action = "na.pass")
# Merge the dummy variables with the original dataset
data <- cbind(data, rel_dummies)

# data we'll use in the analysis
data_an <-  data |> # subsets only variables necessary for analysis
  filter(!is.na(sigi))|>
  select(-c(country, code, region, fos, sigid, gdp, pop, relMissing)) 


# descriptives ------------------------------------------------------------

# What is the relationship between sigi and democracy?
data |>
  ggplot(aes(x = dem, y = sigi)) +
  geom_point() +
  geom_smooth()

#general summary
summary(data)

# correlations between variables
data |>
  select(cpi, fragility, gdp, gini, lifeexp, oilexp, pop, sigi, urb) |>
  ggpairs()

# Boxplots
data %>%
  select(cpi, fragility, gdp, gini, lifeexp, oilexp, pop, sigi, urb) %>%
  tidyr::pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Variable, y = Value)) +
  geom_boxplot() +
  labs(x = "Variable", y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Variable, scales = "free", nrow = 3)

# check NAs -> when we focus on SIGI, few NA on other stuff
filtered_data <- data %>%
  filter(!is.na(sigi))

na_counts <- filtered_data %>%
  summarise(across(everything(), ~ sum(is.na(.))))

print(na_counts)

# densities for cpi
filtered_data |>
  filter(!is.na(fos)) |> # belieze, somalia, south sudan miss some values
  ggplot(aes(x = cpi, fill = fos)) +
  geom_density(alpha = 0.5)

# densities for all variables
filtered_data %>%
  filter(!is.na(fos)) |>
  pivot_longer(cols = c(cpi, fragility, gdp, gini, lifeexp, oilexp, pop, sigi, urb),
               names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Value, fill = fos)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Variable, scales = "free")

# how is the SIGI variable distributed?
data |>
  ggplot(aes(x = sigi)) +
  geom_density()

data |> 
  ggplot(aes(y = sigi)) +
  geom_boxplot() # median is around 26; IQR between 17 and 41

# Find appropriate threshold for unsupervised methods:
filtered_data |>
  filter(sigi > 35) |> # maybe 35 could make sense? I should find an underlyin
  nrow()               # theory to justify a number: check OECD website


# split train and test ----------------------------------------------------

set.seed(1)

train_index =sample(c(TRUE,FALSE), nrow(data_an),rep=TRUE, prob = c(0.8,0.2))
data_an <- cbind(train_index,data_an)
table(data_an$train)

train <- data_an |>
  filter(train_index == 'TRUE')
test <- data_an |>
  filter(train_index == 'FALSE')

train = train[,-train_index] # remove the train_index column
test = test[,-train_index]

dim(train)
dim(test)

# supervised models ----------------------------------------------------------------

# Select best model
regfit.full=regsubsets(sigi~.,data=train,nvmax=11) # all models
reg.summary <- summary(regfit.full) 

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
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")  # 8 vars
plot(regfit.full,scale="Cp")     # 5 vars
plot(regfit.full,scale="bic")    # 4 vars
coef(regfit.full,6) # best according to Cp

test.mat=model.matrix(sigi~.,data=test)
val.errors=rep(NA,6)

ols_robust = lm_robust(sigi ~ cpi+opec+fragility+gini+rel,
                       data = train, se_type = "HC2")
summary(ols_robust)

## Robust ols with rlm by MASS
ols_robust_test_predictions = predict(ols_robust, newdata = test)
mse(fitted(ols_robust), train, "Class") #training error
mse(ols_robust_test_predictions, test, "Class") #test error




# mse function
mse = function(predictions,data,y){
  residuals = (predictions - (data[c(y)]))
  mse = (1/nrow(data))*sum((residuals^2))
  return(mse)
}

# Ridge regression
# Lasso regression

# following code is from group project
# 4. Ridge
X = model.matrix(sigi~.-1, data = data_an[train,])
y=train$Class
ridge=glmnet(X,y,alpha=0)
ridge$beta
plot(ridge,xvar="lambda", label = TRUE)
ridge_fitted = predict(ridge, newx = X) # fitted value for the training set using the best lambda value automatically selected by the function
ridge_predicted = predict(ridge, newx = model.matrix(Class~.-1, data = test)) # fitted value for the training set using the best lambda value automatically selected by the function
cv.ridge=cv.glmnet(X,y,alpha=0)
coef(cv.ridge)
plot(cv.ridge) # cv mse of the ridge
cv.ridge_predicted = predict(cv.ridge, newx = X)
mse(ridge_fitted, train, "Class") # training error of the ridge
mse(ridge_predicted, test, "Class") # test error of the ridge
mse(cv.ridge_predicted, test, "Class") # cv test error of the ridge

# 5. Lasso
fit.lasso=glmnet(x,y)
plot(fit.lasso,xvar="lambda",label=TRUE)
cv.lasso=cv.glmnet(x,y)
plot(cv.lasso)
coef(cv.lasso)
mse(fit.lasso, raisins, "Class")
predict(fit.lasso,newx = x)




# multivariate regression
mod1 =lm(sigi~
          cpi+dem+opec+fragility+gdp+gini+lifeexp+oilexp+pop+rel+urb,data=data)
summary(mod1)
par(mfrow=c(2,2))
plot(mod1)

# robust regression
mod1r <- lm_robust(sigi~
            cpi+dem+opec+fragility+gdp+gini+lifeexp+oilexp+pop+rel+urb,data=data,
            se_type = "stata")
summary(mod1r)



# diagnostics -------------------------------------------------------------

# cook's distance, leverage, forward search
# unsupervised models -----------------------------------------------------

# PCA, then KMeans
