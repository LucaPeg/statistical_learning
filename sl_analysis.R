
# libraries ---------------------------------------------------------------


library(OECD)
library(tidyverse)
library(readxl)


# data import -------------------------------------------------------------


sigi_data <- read.csv("sigi2023.csv")  #oecd
sigi_data <- sigi_data |>
  filter(VAR == "SIGI") |>
  filter(REGION != "REG1_5") |>
  filter(!Country %in% c("Africa", "Asia", "Europe", "Americas"))

gdp_data <- read_excel("pppgdp.xlsx") #worldbank

gini_data <- read_excel("gini.xls") # worldbank




