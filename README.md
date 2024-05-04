## Welcome!
This is the project's repository for the "Statistical Learning" course of DSE@unimi.
The project focuses on understanding whether, given some socio-economic variables, we can assess the level of gender discrimination at the country level.

## Data
To measure the degree of gender discrimination we use the SIGI (Social Institutions & Gender Index) developed by the OECD.
The SIGI will therefore be the dependent variable, whereas as independent variables we will use:
- GDP (WorldBank)
- gini: Gini index (WorldBank)
- dem: Democracy Index (Economist Intelligence Unit)
- fragility: Fragility Index (Fund for peace)
- lifeexp: Life Expectancy (WorldBank)
- urb: Urbanization rate (WorldBank)
- oilexp: Exports of crude oil (CIA)
- pop: Population (Worldbank)
- cpi: Corruption Perception Index (transparency.org)
- rel: Most common religion in the country (World Population Review)

## Techniques
After merging the dataset and creating a few derivative variables, we employ supervised/unsupervised models to predict the countries' SIGI index.
#### Supervised Techniques
- Best Subset Selection
- Ridge Regression
- Lasso Regression
#### Unsupervised Techniques
- PCA
- K-Means of PCA's output
