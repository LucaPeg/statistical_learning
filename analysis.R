
# libraries ---------------------------------------------------------------

library(tidyverse)
library(readr)
library(ggplot2)
library(GGally)
library(estimatr)

# import data -------------------------------------------------------------

data <-  read.csv("~/GitHub/sl_exam/statistical_learning/sigi_dataset.csv", sep=";")

# descriptives ------------------------------------------------------------

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


# supervised models ----------------------------------------------------------------





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
