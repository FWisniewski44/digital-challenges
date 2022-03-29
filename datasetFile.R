#########################################################################
# libraries

library(tidyverse)
library(tidymodels)
library(readr)
library(MASS)
library(broom)
library(Amelia)
library(Hmisc)
library(summarytools)
library(d3heatmap)
library(car)
library(haven)

# set wd
setwd(dir = "/Users/flo/Documents/Uni/Uni Bamberg/WS21-22/cs2-digital-challenges/digital-challenges")

#########################################################################

# load data set --- evs2017
evs2017 <- haven::read_sav(file = "evs2017.sav")
