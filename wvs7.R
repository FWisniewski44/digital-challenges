# same tests and models but with wvs wave 7

# libraries
library(memisc)
library(pander)
library(tidyverse)
library(tidymodels)
library(easystats)
library(parameters)
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
library(epiDisplay)
library(sjmisc)
library(sjPlot)
library(report)

load(file = "/Users/flo/Downloads/WVS_Cross-National_Wave_7_Rdata_v3_0.rdata")
wvs7 <- `WVS_Cross-National_Wave_7_R_v3_0`
wvs7 <- as_tibble(wvs7)

fre(wvs7$Q45)

# dependent variable: expert government
fre(wvs7$Q236)
wvs7 <- wvs7 %>% dplyr::rename(expert = Q236)
fre(wvs7$expert)
# wvs7$B_COUNTRY_ALPHA

# select the variables according to often-tested ones
# also, my own selection about perceptions of science & technology and views of religion/faith
wvs7Subset <- subset(wvs7, select = c("expert", "Q112", "GDPpercap1", "Q260", "Q262", "Q275R",
                                      "Q250", "Q252", "Q240", "Q199", "Q82_EU", "Q71", "Q72", "Q73",
                                      "Q158", "Q159", "Q160", "Q161", "Q163", "Q164", "Q169", "Q173",
                                      "B_COUNTRY_ALPHA", "Q288R", "Q94R", "Q95R", "Q96R", "Q101R"))

# setting better variable names
wvs7Subset <- wvs7Subset %>% dplyr::rename(corrPerc = Q112, GDPpc = GDPpercap1, sex = Q260, age = Q262, educ = Q275R,
                      attDem = Q250, satDem = Q252, lrScale = Q240, polInt = Q199, trustEU = Q82_EU,
                      trustGov = Q71, trustParties = Q72, trustParl = Q73,
                      sciLife = Q158, sciOpp = Q159, sciDepend = Q160, sciRight = Q161, sciWorld = Q163, impGod = Q164,
                      sciVSrel = Q169, religiosity = Q173, country = B_COUNTRY_ALPHA,
                      incc = Q288R, memChurch = Q94R, memSport = Q95R, memArt = Q96R, memCharity = Q101R)

########## adjust scales

# expert: technocratic attitudes of citizens
wvs7Subset$expert <- as.factor(wvs7Subset$expert)
var_lab(wvs7Subset$expert) <- "Attitude towards Technocracy"
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

# alternative: satisfaction with democracy as in Chiru & Enyedi: satDem
## Chiru & Enyedi capture it with an 11-point scale, just like this
## the wording in WVS is: how satisfied are you with how the political system is functioning in your country
## these days? ---> so this is not exactly "democracy", but: with the country selection, we can infer, that
## (at least from the constitutions), they are democracies
wvs7Subset$satDem <- as.numeric(wvs7Subset$satDem)
var_lab(wvs7Subset$satDem) <- "Satisfaction with Democracy per Respondent"

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

############################### memberships of citizens for social capital index
fre(wvs7Subset$memSport)

## all as factors
wvs7Subset$memArt <- as.factor(wvs7Subset$memArt)
var_lab(wvs7Subset$memArt) <- "Respondent: Member of Art/Music/Educational Organization"
wvs7Subset$memChurch <- as.factor(wvs7Subset$memChurch)
var_lab(wvs7Subset$memChurch) <- "Respondent: Member of Church/Religious Organization"
wvs7Subset$memSport <- as.factor(wvs7Subset$memSport)
var_lab(wvs7Subset$memSport) <- "Respondent: Member of Sports/Recreational Organization"
wvs7Subset$memCharity <- as.factor(wvs7Subset$memCharity)
var_lab(wvs7Subset$memCharity) <- "Respondent: Member of Charitable/Humanitarian Organization"

## combine levels: 0 = no member; 1 = member
wvs7Subset$memArt <- combineLevels(fac = wvs7Subset$memArt,
                                     levs = c("-5", "-2", "-1"), newLabel = NA)
wvs7Subset$memChurch <- combineLevels(fac = wvs7Subset$memChurch,
                                   levs = c("-5", "-4", "-2", "-1"), newLabel = NA)
wvs7Subset$memSport <- combineLevels(fac = wvs7Subset$memSport,
                                   levs = c("-5", "-2", "-1"), newLabel = NA)
wvs7Subset$memCharity <- combineLevels(fac = wvs7Subset$memCharity,
                                   levs = c("-5", "-2", "-1"), newLabel = NA)


############################### missing check and imputations before index calculation

# # missing check
# md.pattern(wvs7Subset)
# 
# missing_plot_countries <- aggr(wvs7Subset, col=c("blue", "red"),
#                                numbers=T, sortVars=T, labels=names(wvs7Subset),
#                                cex.axis=0.7, gap=3, ylab=c("missing data", "pattern"))
# missing_plot
# 
# # imputation of missings
# imputed_countries <- mice(wvs7Subset,
#                           m = 5, maxit = 5,
#                           method = "pmm",
#                           seed = 500)
# summary(imputed_countries)
# 
# ## save the step, computationally expensive
# save(imputed_countries, "wvs_imputation.RData")

############################## build indices

# index 1: political trust of citizens: 13 - values, to invert the count
## before: individual vars was 1 = high trust, 4 = low trust; this is confusing for interpretation of models
## after: 13-X leads to index with 10 levels, ranging from 1 = low trust to 10 = high trust
wvs7Subset$polTrust <- 13-(as.numeric(wvs7Subset$trustParl) +
                             as.numeric(wvs7Subset$trustParties) +
                             as.numeric(wvs7Subset$trustGov))
wvs7Subset$polTrust <- as.numeric(wvs7Subset$polTrust)
fre(wvs7Subset$polTrust)
var_lab(wvs7Subset$polTrust) <- "Political Trust"

## test internal consistency of index: cronbachs alpha
wvs7Subset$trustParl_num <- as.numeric(wvs7Subset$trustParl)
wvs7Subset$trustParties_num<- as.numeric(wvs7Subset$trustParties)
wvs7Subset$trustGov_num <- as.numeric(wvs7Subset$trustGov)

polTrust_test <- wvs7Subset[, c("trustParl_num", "trustParties_num", "trustGov_num")]
performance::cronbachs_alpha(polTrust_test)
## 0.87 ---> excellent value

# index 2: positive/negative view of science:
## before: sciLife and sciOpp with lower values = science bad and higher values = science good
## this has been left untouched
wvs7Subset$sciAtt <- (as.numeric(wvs7Subset$sciLife) + as.numeric(wvs7Subset$sciOpp)) / 2
fre(wvs7Subset$sciAtt)
var_lab(wvs7Subset$sciAtt) <- "Attitudes towards Science & Technology"

## test for internal consistency: cronbachs alpha
wvs7Subset$sciLife_num <- as.numeric(wvs7Subset$sciLife)
wvs7Subset$sciOpp_num <- as.numeric(wvs7Subset$sciOpp)

sciAtt_test <- wvs7Subset[, c("sciLife_num", "sciOpp_num")]
performance::cronbachs_alpha(sciAtt_test)
## 0.75 ---> also a decent value

# index 3: social capital through memberships in organizations
## before: 0 = no membership, 1 = membership in a certain organization
## after: in index, the scales were subtracted with 3, to norm it to a scale of 1 to 5,
## with 1 = low social capital and 5 = high social capital
wvs7Subset$socCap <- (as.numeric(wvs7Subset$memArt) + 
                        as.numeric(wvs7Subset$memCharity) +
                        as.numeric(wvs7Subset$memChurch) +
                        as.numeric(wvs7Subset$memSport)) - 3
# fre(wvs7Subset$memArt)
var_lab(wvs7Subset$socCap) <- "Social Capital"

## test for internal consistency: cronbachs alpha
wvs7Subset$memArt_num <- as.numeric(wvs7Subset$memArt)
wvs7Subset$memCharity_num <- as.numeric(wvs7Subset$memCharity)
wvs7Subset$memChurch_num <- as.numeric(wvs7Subset$memChurch)
wvs7Subset$memSport_num <- as.numeric(wvs7Subset$memSport)

socCap_test <- wvs7Subset[, c("memArt_num", "memCharity_num", "memChurch_num", "memSport_num")]
performance::cronbachs_alpha(socCap_test)
## rounded to 0.71 ---> still a decent value

############################### subset creation and index creation

## selecting subset of just european countries included in the data set
wvs7_countries <- wvs7Subset %>% dplyr::filter(country %in% c("ALB", "AND", "AUT", "BLR", "BIH", "BGR", "HRV",
                                                 "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU",
                                                 "GRC", "HUN", "ISL", "ITA", "LTU", "MNE", "NLD",
                                                 "MKD", "NOR", "POL", "PRT", "ROU", "SRB", "SVK",
                                                 "SVN", "ESP", "SWE", "CHE", "UKR", "GBR"))

# check the tibble
wvs7Subset
fre(wvs7_countries$country)
# there are not a lot of european countries included

# wvs7_countries$expert <- combineLevels(wvs7_countries$expert, levs = c("-5", "-2", "-1"), newLabel = NA)

wvs7Chiru <- wvs7Subset %>% dplyr::filter(country %in% c("NLD", "DEU", "GBR", "CHE", "GRC", "HUN", "ROU",
                                                                 "BRA", "ARG"))
fre(wvs7Chiru$country) ## not included bc. of data availability: NL, UK, CH, HUN

############# dichotomization of dependent variable "expert" for technocracy attitudes
wvs7Chiru$expert_bin <- combineLevels(wvs7Chiru$expert, levs = c("1", "2"), newLabel = "1")
wvs7Chiru$expert_bin <- combineLevels(wvs7Chiru$expert_bin, levs = c("3", "4"), newLabel = "0")
fre(wvs7Chiru$expert)
fre(wvs7Chiru$expert_bin)
levels(wvs7Chiru$expert_bin)

save(wvs7Chiru, file = "wvs7Chiru.RData")

## data set 1: Germany
wvs7GER <- wvs7Chiru %>% dplyr::filter(country %in% c("DEU"))

## data set 2: Greece
wvs7GRC <- wvs7Chiru %>% dplyr::filter(country %in% c("GRC"))

# ## data set 3: Hungary
# wvs7HUN <- wvs7Chiru %>% dplyr::filter(country %in% c("HUN"))

## data set 4: Romania
wvs7ROU <- wvs7Chiru %>% dplyr::filter(country %in% c("ROU"))

## data set 5: Brazil
wvs7BRA <- wvs7Chiru %>% dplyr::filter(country %in% c("BRA"))

## data set 6: Argentina
wvs7ARG <- wvs7Chiru %>% dplyr::filter(country %in% c("ARG"))

#################################### regression models (testing here before inclusion into paper)

# modAll <- glm(data = wvs7Chiru, formula = expert_bin ~ lrScale + polInt + polTrust +
#               sex + age + educ + incc + socCap + sciAtt,
#             family = "binomial")

modGER <- glm(data = wvs7GER, formula = expert_bin ~ lrScale + corrPerc + polInt + polTrust +
                satDem + age + educ + incc + socCap + sciAtt,
              family = "binomial")
# summary(glm(data = wvs7GER, formula = expert_bin~attDem, family = "binomial"))
# 
# summary(modGER)

modGRC <- glm(data = wvs7GRC, formula = expert_bin ~ lrScale + corrPerc + polInt + polTrust +
                satDem + age + educ + incc + socCap + sciAtt,
              family = "binomial")

# modHUN <- glm(data = wvs7HUN, formula = expert_bin ~ lrScale + polInt + polTrust +
#                 sex + age + educ + incc + socCap + sciAtt,
#               family = "binomial")

modROU <- glm(data = wvs7ROU, formula = expert_bin ~ lrScale + corrPerc + polInt + polTrust +
                satDem + age + educ + incc + socCap + sciAtt,
              family = "binomial")

modBRA <- glm(data = wvs7BRA, formula = expert_bin ~ lrScale + corrPerc + polInt + polTrust +
                satDem + age + educ + incc + socCap + sciAtt,
              family = "binomial")
summary(modBRA)

modARG <- glm(data = wvs7ARG, formula = expert_bin ~ lrScale +corrPerc + polInt + polTrust +
                satDem + age + educ + incc + socCap + sciAtt,
              family = "binomial")
# summary(modARG)

## test for displaying results:
## make a combined markdown table of all models with memisc and pander
combTable <- mtable('Germany' = modGER,
                    'Greece' = modGRC,
                    #'Hungary' = modHUN,
                    'Romania' = modROU,
                    'Brazil' = modBRA,
                    'Argentina' = modARG,
                    summary.stats = c('R-squared', 'F', 'p', 'N'))
library(sjPlot)


### tab of models, log odds displayed
tab_model(modGER, modGRC, modROU, modBRA, modARG, p.style = "numeric_stars",
          title = "Log-odds: Regression models for Germany, Greece, Romania, Brazil, and Argentina",
          dv.labels = c("Germany", "Greece", "Romania", "Brazil", "Argentina"),
          transform = NULL,
          file = "output_LO.html")

## tab of models, odds ratios displayed
tab_model(modGER, modGRC, modROU, modBRA, modARG, p.style = "numeric_stars",
                     title = "Odds ratios: Regression models for Germany, Greece, Romania, Brazil, and Argentina",
                     dv.labels = c("Germany", "Greece", "Romania", "Brazil", "Argentina"),
                    file = "output_OR.html")

## graphical/visual models
### all in one
plot_models(modGER, modGRC, modROU, modBRA, modARG, show.p = T)
print_md(parameters(modGER))

### univariate display of variables (especially for technocracy attitudes!)
#### recode expert variable so it has value labels
wvs7Chiru$expert_lab <- factor(wvs7Chiru$expert, levels = c(1, 2, 3, 4), labels = c("Very good", "Fairly good", "Fairly Bad", "Very bad"))
freq(wvs7Chiru$expert_lab)

sjPlot::plot_frq(wvs7Chiru$expert)
sjPlot::plot_frq(wvs7Chiru$expert_bin)

## testing the report package
report(modGER)




fre(wvs7GER$satDem)

wvs7Chiru$country_re <- factor(wvs7Chiru$country, levels = c("ROU", "BRA", "ARG", "DEU", "GRC"))
fre(wvs7Chiru$country_re)

save(wvs7Chiru, file = "wvs7Chiru.RData")

ggplot(data = wvs7Chiru) + 
  geom_bar(aes(y = country_re, fill = expert_bin), show.legend = T, position = "fill") + 
  ylab(label = "Countries") + xlab("Percentages for citizens' attitudes towards technocracy per country")

