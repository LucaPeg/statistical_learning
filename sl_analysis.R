
# libraries ---------------------------------------------------------------


library(OECD)
library(tidyverse)
library(readxl)
library(readr)
# data import -------------------------------------------------------------


sigi_data <- read.csv("sigi2023.csv")  #oecd
sigi_data <- sigi_data |>
  filter(VAR == "SIGI") |>
  filter(REGION != "REG1_5") |>
  filter(!Country %in% c("Africa", "Asia", "Europe", "Americas"))

gdp_data <- read_excel("pppgdp.xlsx") #worldbank

gini_data <- read_excel("gini.xls") # worldbank

dem_data <- read.csv("dem_eiu.csv") |> #economist intelligence unit
  filter(Year == 2022)

fragility_data <- read.csv("fragility.csv") # fund for peace

lifeexp_data <- read_excel("life_exp.xls") # worldbank

urb_data <- read_excel("urbanization.xls") # SP.URB.TOTL.IN.ZS worldbank

pop_data <- read_excel("pop.xls")# SP.POP.TOTL worldbank

oilexp_data <- read.csv("oil_export.csv") # CIA

dem_data$opec <- ifelse(
  dem_data$Entity %in% c("Algeria","Congo","Equatorial Guinea","Gabon","Iran",
                         "Iraq","Kuwait","Libya","Nigeria","Saudi Arabia",
                         "United Arab Emirates","Venezuela"),1,0)   

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

