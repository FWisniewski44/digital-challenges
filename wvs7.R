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

########## adjust scales

# expert: technocratic attitudes of citizens
wvs7Subset$expert <- as.factor(wvs7Subset$expert)
var_lab(evs_subset$techno) <- "Attitude towards Technocracy"
wvs7Subset$expert <- combineLevels(fac = wvs7Subset$expert, levs = c("-5", "-2", "-1"), newLabel = NA)

# corrPerc: country level var --- corruption perception in country
wvs7Subset$corrPerc <- as.numeric(wvs7Subset$corrPerc)
var_lab(wvs7Subset$corrPerc) <- "Perception of Corruption in Respondents' Respective Country"

# GDPpc: country level var --- GDP per capita
wvs7Subset$GDPpc <- as.numeric(wvs7Subset$GDPpc)
var_lab(wvs7Subset$GDPpc) <- "GDP per capita in Respondents' Respective Country"

# context vars: sex, age, educ
wvs7Subset$sex <- as.factor(wvs7Subset$sex)
var_lab(wvs7Subset$sex) <- "Respondents' Sex"
wvs7Subset$sex <- combineLevels(fac = wvs7Subset$sex, levs = c("-5", "-2", "-1"), newLabel = NA)

wvs7Subset$age <- as.numeric(wvs7Subset$age)
var_lab(wvs7Subset$age) <- "Respondents' Age"

wvs7Subset$educ <- as.factor(wvs7Subset$educ)
var_lab(wvs7Subset$educ) <- "Respondents' Education Level"
wvs7Subset$educ <- combineLevels(fac = wvs7Subset$educ, levs = c("-5", "-2", "-1"), newLabel = NA)


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

# # trustEU: trust in the EU
# wvs7Subset$trustEU <- as.factor(wvs7Subset$trustEU)
# var_lab(wvs7Subset$trustEU) <- "Trust in the EU"
# wvs7Subset$trustGov <- combineLevels(fac = wvs7Subset$trustGov, levs = c("-4", "-2", "-1"), newLabel = NA)

######## trust variables

# trust in national government, parties and parliament
wvs7Subset$trustGov <- as.factor(wvs7Subset$trustGov)
var_lab(wvs7Subset$trustGov) <- "Trust in National Government"

wvs7Subset$trustParties <- as.factor(wvs7Subset$trustParties)
var_lab(wvs7Subset$trustParties) <- "Trust in Parties"

wvs7Subset$trustParl <- as.factor(wvs7Subset$trustParl)
var_lab(wvs7Subset$trustParl) <- "Trust in Parliament"

### set the negative values (which represent NAs, but with background meaning) to NA
wvs7Subset$trustParl <- combineLevels(fac = wvs7Subset$trustParl, levs = c("-5", "-2", "-1"), newLabel = NA)
wvs7Subset$trustParties <- combineLevels(fac = wvs7Subset$trustParties, levs = c("-5", "-2", "-1"), newLabel = NA)
wvs7Subset$trustGov <- combineLevels(fac = wvs7Subset$trustGov, levs = c("-5", "-4", "-2", "-1"), newLabel = NA)

########################################### Science and Technology attitudes

## sciLife: science & tech will help us lead a healthier, easier, better life
wvs7Subset$sciLife <- as.factor(wvs7Subset$sciLife)
var_lab(wvs7Subset$sciLife) <- "Science & Technology: Help with Healthier, Easier, Better Life"

## sciOpp: science & tech will lead to more oppportunities for next generations
wvs7Subset$sciOpp <- as.factor(wvs7Subset$sciOpp)
var_lab(wvs7Subset$sciOpp) <- "Science & Technology: Create Opportunities for Next Generations"

## sciDepend: "we depend too much on science and not enough on faith"
wvs7Subset$sciDepend <- as.factor(wvs7Subset$sciDepend)
var_lab(wvs7Subset$sciDepend) <- "Science & Technology: too much dependence, not enough faith"

## sciRight: people forget what is right and wrong because of science and tech
wvs7Subset$sciRight <- as.factor(wvs7Subset$sciRight)
var_lab(wvs7Subset$sciRight) <- "Science & Technology: people forget what's right/wrong"

## sciWorld: world with science --- better or worse off? (1=worse; 10=better)
wvs7Subset$sciWorld <- as.factor(wvs7Subset$sciWorld)
var_lab(wvs7Subset$sciWorld) <- "World with Science: Better/Worse off?"

## recode the science vars: different missings into one category of NA
wvs7Subset$sciLife <- combineLevels(fac = wvs7Subset$sciLife,
                                    levs = c("-5", "-4", "-2", "-1"), newLabel = NA)
wvs7Subset$sciOpp <- combineLevels(fac = wvs7Subset$sciOpp,
                                   levs = c("-5", "-4", "-2", "-1"), newLabel = NA)
wvs7Subset$sciDepend <- combineLevels(fac = wvs7Subset$sciDepend,
                                   levs = c("-5", "-4", "-2", "-1"), newLabel = NA)
wvs7Subset$sciRight <- combineLevels(fac = wvs7Subset$sciRight,
                                   levs = c("-5", "-4", "-2", "-1"), newLabel = NA)
wvs7Subset$sciWorld <- combineLevels(fac = wvs7Subset$sciWorld,
                                   levs = c("-5", "-4", "-2", "-1"), newLabel = NA)

########################################### Faith and Religion attitudes

## impGod: importance of god in respondents' lives
wvs7Subset$impGod <- as.factor(wvs7Subset$impGod)
var_lab(wvs7Subset$impGod) <- "Importance of God in Respondents' Lives"

## sciVSrel: in a conflict of science vs. religion, religion is always right
wvs7Subset$sciVSrel <- as.factor(wvs7Subset$sciVSrel)
var_lab(wvs7Subset$sciVSrel) <- "Conflict Science vs. Religion: Religion always right"

## religiosity: religiosity of respondent, self-perceived
wvs7Subset$religiosity <- as.factor(wvs7Subset$religiosity)
var_lab(wvs7Subset$religiosity) <- "Respondents' Religiosity"

## recoding faith vars: different missings categories into NA
wvs7Subset$impGod <- combineLevels(fac = wvs7Subset$impGod,
                                     levs = c("-5", "-4", "-2", "-1"), newLabel = NA)
wvs7Subset$sciVSrel <- combineLevels(fac = wvs7Subset$sciVSrel,
                                     levs = c("-5", "-2", "-1"), newLabel = NA)
wvs7Subset$religiosity <- combineLevels(fac = wvs7Subset$religiosity,
                                     levs = c("-5", "-2", "-1"), newLabel = NA)

## country: home countries of respondents
wvs7Subset$country <- as.factor(wvs7Subset$country)
var_lab(wvs7Subset$country) <- "Home Countries of Respondents"
fre(wvs7Subset$country)

############################### missing check and imputations before index calculation

# missing check
md.pattern(wvs7Subset)

missing_plot_countries <- aggr(wvs7Subset, col=c("blue", "red"),
                               numbers=T, sortVars=T, labels=names(wvs7Subset),
                               cex.axis=0.7, gap=3, ylab=c("missing data", "pattern"))
missing_plot

# imputation of missings
imputed_countries <- mice(wvs7Subset,
                          m = 5, maxit = 5,
                          method = "pmm",
                          seed = 500)
summary(imputed_countries)

save(imputed_countries, "wvs_imputation.RData")

############################### subset creation and index creation

## selecting subset of european countries included in the data set
wvs7_countries <- wvs7Subset %>% dplyr::filter(country %in% c("ALB", "AND", "AUT", "BLR", "BIH", "BGR", "HRV",
                                                 "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU",
                                                 "GRC", "HUN", "ISL", "ITA", "LTU", "MNE", "NLD",
                                                 "MKD", "NOR", "POL", "PRT", "ROU", "SRB", "SVK",
                                                 "SVN", "ESP", "SWE", "CHE", "UKR", "GBR"))

# check the tibble
wvs7Subset
fre(wvs7_countries$country)

wvs7_countries$expert <- combineLevels(wvs7_countries$expert, levs = c("-5", "-2", "-1"), newLabel = NA)
wvs7_countries$expert <- combineLevels(wvs7_countries$expert, levs = c("1", "2"), newLabel = "positive")
wvs7_countries$expert <- combineLevels(wvs7_countries$expert, levs = c("3", "4"), newLabel = "negative")

fre(wvs7_countries$sciLife)


# after imputation, check

wvs7Subset$sciAtt <- (as.numeric(wvs7Subset$sciLife) + as.numeric(wvs7Subset$sciOpp))
fre(wvs7Subset$sciOpp)

## index: political trust of citizens
wvs7Subset$polTrust <- 13-(as.numeric(wvs7Subset$trustParl) +
                             as.numeric(wvs7Subset$trustParties) +
                             as.numeric(wvs7Subset$trustGov))

wvs7Subset$polTrust <- as.factor(wvs7Subset$polTrust)
var_lab(wvs7Subset$polTrust) <- "Trust in Political Institutions"
fre(wvs7Subset$polTrust)


# index sciView: positive/negative view of science

