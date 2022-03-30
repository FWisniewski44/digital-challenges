#########################################################################

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

# set wd
setwd(dir = "/Users/flo/Documents/Uni/Uni Bamberg/WS21-22/cs2-digital-challenges/digital-challenges")

######################################################################### loading the data and specifying vars

# load data set --- evs2017
evs2017 <- haven::read_sav(file = "evs2017.sav")
evs2017 <- as_tibble(evs2017)

## dependent var: "Having experts, not government, make decisions according to what they think is best for the country"
## v146; renamed to "techno"

fre(evs2017$v146)
evs2017 <- evs2017 %>% rename(techno = v146)
fre(evs2017$techno)

# we need the following variables that Bertsou/Pastorella used, too:

## democratic attitude index
#### index variable, vars not found in evs2017; v142 taken instead ("importance of democracy")

## political trust index
#### index variable, consists of: v121 (parliament) --- v130 (political parties) --- v131 (government)

## trust in EU ---> v124

## political interest ---> v97

## left-right ideology (quasi-metric---> v102

## sex (ref. cat. male) (unordered) ---> v225

## education (ordered) ---> v243_r (for low medium high education) or v243_EISCED (for ISCED codes 1-8)

## age (numeric) ---> v226

# Bertsou/Pastorella also used country level variables:

## CPI index (corruption perception index) via external data source

## GDP per capita via external data source

## technocratic regime experienced before ---> McDonnell/Valbruzzi (2014) have a list

## communist regime experienced before ---> nothing mentioned, this could be deduced from the data + history

######################################################################### dealing with missing data

## a lot of NAs were detected in the data, which should not just be omitted
## therefore, imputation shall be done via the package MICE

# first step: reduce dimensionality by creating a subset of just the required variables
evs_subset <- subset(evs2017, select = c("country", "techno", "v121", "v130", "v131",
                                         "v124", "v97", "v102", "age", "v243_r", "v243_EISCED", "v142"))

evs_subset <- evs_subset %>% rename(trustParl = v121, trustParties = v130, trustGov = v131,
                                    trustEU = v124, polInt = v97, lrScale = v102,
                                    educ = v243_r, educEISCED = v243_EISCED, attDemoc = v142)

## second step: display the pattern of the missing values
md.pattern(evs_subset)

missing_plot <- aggr(evs_subset, col=c("blue", "red"),
                     numbers=T, sortVars=T, labels=names(evs_subset),
                     cex.axis=0.7, gap=3, ylab=c("missing data", "pattern"))
missing_plot

## third step: imputation of data

## preparation step: set scales
## technocracy attitudes
evs_subset$techno <- as.factor(evs_subset$techno)
var_lab(evs_subset$techno) <- "Attitude towards Technocracy"

## attitude towards democracy
evs_subset$attDemoc <- as.factor(evs_subset$attDemoc)
var_lab(evs_subset$trustParl) <- "Attitude towards Democracy: how important is D. for Respondent"

## political trust: (for creation of index ---> in the beginning: higher value = less trust)
evs_subset$trustParl <- as.factor(evs_subset$trustParl)
var_lab(evs_subset$trustParl) <- "Trust in Parliament"
evs_subset$trustParties <- as.factor(evs_subset$trustParties)
var_lab(evs_subset$trustParties) <- "Trust in Political Parties"
evs_subset$trustGov <- as.factor(evs_subset$trustGov)
var_lab(evs_subset$trustGov) <- "Trust in Government"

## trust in EU
evs_subset$trustEU <- as.factor(evs_subset$trustEU)
var_lab(evs_subset$trustEU) <- "Trust in EU"

## political interest
evs_subset$polInt <- as.factor(evs_subset$polInt)
var_lab(evs_subset$polInt) <- "Political interest"

## left-right scale
evs_subset$lrScale <- as.factor(evs_subset$lrScale)
var_lab(evs_subset$lrScale) <- "Political Left-Right Scale, Respondents' self-placement"

## age
evs_subset$age <- as.numeric(evs_subset$age)
var_lab(evs_subset$age) <- "Age of Respondent"

## educational level
evs_subset$educ <- as.factor(evs_subset$educ)
var_lab(evs_subset$educ) <- "Educational Level, Threefold Scale"

evs_subset$educEISCED <- as.factor(evs_subset$educEISCED)
var_lab(evs_subset$educEISCED) <- "Educational Level, Detailed Scale"


## actual imputation via mice package
imputed <- mice(evs_subset,
     m = 5, maxit = 5,
     method = c("", "polyreg", "polyreg", "polyreg", "polyreg", "polyreg", "polyreg",
                "pmm", "norm.boot", "polr", "polr", "pmm"),
     seed = 500)

save(imputed, file = "impute_step.RData") ## to save time when R crashes and it has to be loaded again

summary(imputed)
imputed$imp$lrScale
evs_imputed <- complete(imputed, 3)
evs_imputed <- as_tibble(evs_imputed)
save(evs_imputed, file = "evs_imputed.RData")

# after everything is imputed:
# creation of an index for political trust by adding trustParl, trustParties and trustGov together
## 13 is chosen as to invert the scale; now, higher values = higher trust
evs_subset$polTrust <- 13 - (as.numeric(evs_subset$trustParl) +
                               as.numeric(evs_subset$trustParties) +
                               as.numeric(evs_subset$trustGov))
fre(evs_subset$polTrust) ## proof of concept: low value = low trust --- high value = high trust

# evs_subset$polTrust <- as.factor(evs_subset$polTrust)
# var_lab(evs_subset$polTrust) <- "Trust in Political System"

## adding the index to evs_imputed, also countries
evs_imputed$polTrust <- evs_subset$polTrust
evs_imputed$country <- as.factor(evs_imputed$country)

# fitting a first model to see how it works
test <- glmer(data = evs_imputed, formula = techno ~ evs_imputed$polTrust + evs_imputed$attDemoc + evs_imputed$educ +
        evs_imputed$age + evs_imputed$lrScale + evs_imputed$polInt + evs_imputed$trustEU + 
        (1 | evs_imputed$country), family = binomial("logit"))



