
# libraries ---------------------------------------------------------------
library(OECD)
library(tidyverse)
library(readxl)
library(writexl)
library(readr)
library(countrycode)

# data import -------------------------------------------------------------


sigi_data <- read.csv("sigi2023.csv")  #oecd
sigi_data <- sigi_data |>
  filter(VAR == "SIGI") |>
  filter(REGION != "REG1_5") |>
  filter(!Country %in% c("Africa", "Asia", "Europe", "Americas"))

gdp_data <- read_excel("gdp2.xlsx")
gdp_data$gdp <- gsub(",", "", gdp_data$gdp)
gdp_data$gdp <- as.numeric(gdp_data$gdp)
gdp_data$code <- countrycode(gdp_data$country, origin = "country.name", destination = "iso3c")

gini_data <- read_excel("gini.xls") # worldbank

dem_data <- read.csv("dem_eiu.csv") |> #economist intelligence unit
  filter(Year == 2022)
dem_data$opec <- ifelse(
  dem_data$Entity %in% c("Algeria","Congo","Equatorial Guinea","Gabon","Iran",
                         "Iraq","Kuwait","Libya","Nigeria","Saudi Arabia",
                         "United Arab Emirates","Venezuela"),1,0)  
dem_data$fos <- ifelse(
  dem_data$democracy_eiu > 5.99, "Democracy", ifelse(
    dem_data$democracy_eiu > 3.00, "Hybrid Regime", "Dictatorship"
))

fragility_data <- read.csv("fragility.csv") # fund for peace

lifeexp_data <- read_excel("life_exp.xls") # worldbank

urb_data <- read_excel("urbanization.xls") # SP.URB.TOTL.IN.ZS worldbank

pop_data <- read_excel("pop.xls")# SP.POP.TOTL worldbank

oilexp_data <- read.csv("oil_export.csv") # CIA
oilexp_data$value <- gsub(",", "", oilexp_data$value)
oilexp_data$value <-  as.numeric(oilexp_data$value)

 

cpi_data <- read_excel("cpi.xlsx") # transparency.org

rel_data <- read.csv("religion.csv") # world population review
colnames(rel_data) = c("country", "total","christians","muslims","unaffiliated",
                       "hindus","buddhists","folk","other","jewish")

rel_data$christians <- (rel_data$christians/rel_data$total) *100
rel_data$muslims <- (rel_data$muslims/rel_data$total) *100
rel_data$unaffiliated <- (rel_data$unaffiliated/rel_data$total) *100
rel_data$hindus <- (rel_data$hindus/rel_data$total) *100
rel_data$buddhists <- (rel_data$buddhists/rel_data$total) *100
rel_data$folk <- (rel_data$folk/rel_data$total) *100
rel_data$other <- (rel_data$other/rel_data$total) *100
rel_data$jewish <- (rel_data$jewish/rel_data$total) *100

# Create a matrix with the columns of interest
count_matrix <- rel_data[, c("christians", "muslims", "unaffiliated", "hindus", "buddhists", "folk", "other", "jewish")]

# Find the column index of the maximum value in each row
max_index <- max.col(count_matrix, ties.method = "first")

# Create a vector of religion names corresponding to the column index
religion_names <- c("Christian", "Muslim", "Unaffiliated", "Hindu", "Buddhist", "Folk", "Other", "Jewish")

# Assign the religion names based on the max_index
rel_data$rel <- religion_names[max_index]

rel_data <- rel_data[, c("country", "rel")]



# change varnames ---------------------------------------------------------

colnames(dem_data) = c("country", "code", "year",'dem','opec', 'fos')
dem_data <- dem_data |> select(code, dem,opec,fos)

colnames(fragility_data) = c("country", 'fragility')
fragility_data$code <- countrycode(sourcevar = fragility_data$country, origin = "country.name", destination = "iso3c")
colnames(gdp_data) = c("country", "code","gdp")
colnames(gini_data) = c("country",'code','gini')
colnames(lifeexp_data) = c('country','code', 'lifeexp')

oilexp_data <-  oilexp_data |> select(name, value)
colnames(oilexp_data) <- c("country", "oilexp")
oilexp_data$code <- countrycode(sourcevar = oilexp_data$country, origin = "country.name", destination = "iso3c")
colnames(oilexp_data)

colnames(pop_data) <-  c("country", "code","pop")
colnames(rel_data)
colnames(sigi_data)

sigi_data <- sigi_data|> select(Region, LOCATION, Value)
colnames(sigi_data) <-  c("region","code","sigi")
sigi_data

colnames(urb_data) <-  c("country",'code','urb')
cpi_data <- cpi_data |> select(iso3, Region, "CPI score 2023")
colnames(cpi_data) <-  c('code','region','cpi')

rel_data$code <- countrycode(sourcevar = rel_data$country, origin = "country.name", destination = "iso3c")

#remove country var

fragility_data <-  fragility_data |>
  select(-country)

gdp_data <- gdp_data |>
  select(-country)

colnames(gdp_data) <- c("gdp","code")

gini_data <- gini_data |>
  select(-country)

lifeexp_data <- lifeexp_data |>
  select(-country)

oilexp_data <- oilexp_data |>
  select(-country)

urb_data <- urb_data |>
  select(-country)

sigi_data <- sigi_data |>
  select(-region)

rel_data <- rel_data |>
  select(-country)


# merge datasets ----------------------------------------------------------

merged_df <- Reduce(function(x, y) merge(x, y, by = "code", all = TRUE), 
                    list(
                      cpi_data,
                      dem_data,
                      fragility_data,
                      gdp_data,
                      gini_data,
                      lifeexp_data,
                      oilexp_data,
                      pop_data,
                      rel_data,
                      sigi_data,
                      urb_data
                    ))

merged_df <- merged_df[-(1:6),]

merged_df <- merged_df |>
  relocate(country)

colSums(is.na(merged_df))

merged_df <- merged_df[complete.cases(merged_df$country), ]

write_csv(merged_df, "project_dataset.csv")
