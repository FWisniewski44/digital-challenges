# same tests and models but with wvs wave 7

# libraries
library(tidyverse)
library(tidymodels)
library(readr)
library(MASS)
library(lme4)
library(broom)
library(Amelia)
library(mice)
library(VIM)
library(Hmisc)
library(summarytools)
library(d3heatmap)
library(car)
library(haven)
library(bookdown)
library(pagedown)
library(ymlthis)
library(esquisse)
library(distill)
library(expss)
library(rockchalk)

load(file = "/Users/flo/Downloads/WVS_Cross-National_Wave_7_Rdata_v3_0.rdata")
wvs7 <- `WVS_Cross-National_Wave_7_R_v3_0`
wvs7 <- as_tibble(wvs7)

fre(wvs7$Q45)

# dependent variable: expert government
fre(wvs7$Q236)
wvs7 <- wvs7 %>% rename(expert = Q236)
fre(wvs7$expert)
wvs7$B_COUNTRY_ALPHA

# select the variables according to often-tested ones
# also, my own selection about perceptions of science & technology and views of religion/faith
wvs7Subset <- subset(wvs7, select = c("expert", "Q112", "GDPpercap1", "Q260", "Q262", "Q275R",
                                      "Q250", "Q240", "Q199", "Q82_EU", "Q71", "Q72", "Q73",
                                      "Q158", "Q159", "Q160", "Q161", "Q163", "Q164", "Q169", "Q173",
                                      "B_COUNTRY_ALPHA"))

# setting better variable names
wvs7Subset <- wvs7Subset %>% rename(corrPerc = Q112, GDPpc = GDPpercap1, sex = Q260, age = Q262, educ = Q275R,
                      attDem = Q250, lrScale = Q240, polInt = Q199, trustEU = Q82_EU, trustGov = Q71, trustParties = Q72, trustParl = Q73,
                      sciLife = Q158, sciOpp = Q159, sciDepend = Q160, sciRight = Q161, sciWorld = Q163, impGod = Q164,
                      sciVSrel = Q169, religiosity = Q173, country = B_COUNTRY_ALPHA)

# check for any missing data --- there are no NAs
md.pattern(wvs7Subset)

missing_plot <- aggr(wvs7Subset, col=c("blue", "red"),
                     numbers=T, sortVars=T, labels=names(wvs7Subset),
                     cex.axis=0.7, gap=3, ylab=c("missing data", "pattern"))
missing_plot

########## adjust scales
wvs7Subset

# expert: technocratic attitudes of citizens
wvs7Subset$expert <- as.factor(wvs7Subset$expert)
var_lab(evs_subset$techno) <- "Attitude towards Technocracy"

# corrPerc: country level var --- corruption perception in country
wvs7Subset$corrPerc <- as.numeric(wvs7Subset$corrPerc)
var_lab(wvs7Subset$corrPerc) <- "Perception of Corruption in Respondents' Respective Country"

# GDPpc: country level var --- GDP per capita
wvs7Subset$GDPpc <- as.numeric(wvs7Subset$GDPpc)
var_lab(wvs7Subset$GDPpc) <- "GDP per capita in Respondents' Respective Country"

# context vars: sex, age, educ
wvs7Subset$sex <- as.factor(wvs7Subset$sex)
var_lab(wvs7Subset$sex) <- "Respondents' Sex"
wvs7Subset$age <- as.numeric(wvs7Subset$age)
var_lab(wvs7Subset$age) <- "Respondents' Age"
wvs7Subset$educ <- as.factor(wvs7Subset$educ)
var_lab(wvs7Subset$edu) <- "Respondents' Education Level"

# attDem: attitude towards democracy
## this is not captured as in Bertsou & Pastorella via an index, but rather via a single variable; wording is:
## "Importance of democracy: How important is it for you to live in a country that is governed democratically?
## On this scale where 1 means it is “not at all important” and 10 means “absolutely important”
## what position would you choose?
wvs7Subset$attDem <- as.numeric(wvs7Subset$attDem)
var_lab(wvs7Subset$attDem) <- "Importance of Democracy for Respondent"

# lrScale: self-ascribed position on left-right scale
wvs7Subset$lrScale <- as.numeric(wvs7Subset$lrScale)
var_lab(wvs7Subset$lrScale) <- "Self-Ascribed Position on Left-Right-Scale"

# polInt: political interest
wvs7Subset$polInt <- as.numeric(wvs7Subset$polInt)
var_lab(wvs7Subset$polInt) <- "Political Interest"

# rest follows





