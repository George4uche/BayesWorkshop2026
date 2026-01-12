
# Sheep model part 1-------------

# Research question: How does lamb genotype affect birth weight?

# This exercise uses the ilri.sheep dataset to demonstrate how to 
# build and interpret a linear model in R. 
# Part 1 uses a frequentist model to illustrate:
#1) exploratory analysis to help build a linear model
#2) the basic structure of a linear model in R
#3) tools we can use to draw inferences from inear models

# Load packages--------
# install any that you don't have
library(agridat)
#library(lme4)
library(tidyverse)
library(emmeans)

# Load ilri.sheep data-----------
data("ilri.sheep")
sheep <- ilri.sheep

# Inspect data --------
## Review documentation --------
?ilri.sheep
str(sheep)

## Prepare data for modeling---------
# are all the continuous variables numeric?
str(sheep) # What variables should be numeric?

# are all the categorical variables factors?
str(sheep)
table(sheep$year)

## year should be a category.
sheep$year <- factor(sheep$year)
unique(sheep$year)
unique(as.character(sheep$year))
unique(as.numeric(sheep$year))
levels(sheep$year) # shows levels of a category. 

# standardize continuous variables

sheep <- sheep %>% 
  mutate(std_birthwt = scale(birthwt, center = TRUE, scale=TRUE)[,1],
         std_weanwt = scale(weanwt, center = TRUE, scale=TRUE)[,1])


hist(sheep$birthwt, bins = 20)
hist(sheep$std_birthwt, bins = 20)

# Build model-----------
# response variable: birthwt
# predictors: gen, sex, year
# possible interaction terms: 
#gen:sex --> 
##    Do the effects of genotype on birthweight 
#      differ between male and female lambs?
# gen:year -->
#     Do the effects of genotype on birth weight 
#     differ among birth year cohorts?
# year:sex -->
#     Do the effects of sex on birth weight differ
#     among birth year cohorts?

# gen:sex:year -->
#   Do the effects of genotype differ between male 
#   and female lambs, and do these effects vary 
#   across birth cohorts?
# which interactions make sense based on our understanding of the system?
# which interactions look likely based on 

# Pre-modeling exploratory analysis ----------
## What is the effect of genotype on birth weight, wean weight and wean age?
## Important covariates: sex, birth year.

## Main effects-----
### genotype vs. birth weight
pgen <- sheep %>%
  drop_na() %>%
  ggplot(aes(x = gen, y = birthwt)) +
  geom_violin()+
  ggforce::geom_sina(alpha= 0.5) 
pgen

sheep %>%
  drop_na() %>%
  ggplot() +
  geom_density(aes(x = birthwt))+
  facet_wrap(vars(gen))

# empirical means
tapply(sheep$birthwt, sheep$gen, mean)
# interpretation:
# FILL IN

### sex vs birth weight
psex <- sheep %>%
  drop_na() %>%
  ggplot(aes(x = sex, y = birthwt)) +
  geom_violin()+
  ggforce::geom_sina(alpha= 0.5) 
psex

#interpretation:
# FILL IN

### year vs birth weight
pyear <- sheep %>%
  drop_na() %>%
  ggplot(aes(x = year, y = birthwt)) +
  geom_violin()+
  ggforce::geom_sina(alpha= 0.5) 
pyear

# interpretation
# FILL IN

## Interactions------
## genotype vs. birth weight separated by sex
# gen:sex interaction
pgensex <- sheep %>%
  drop_na() %>%
  ggplot(aes(x = gen, y = birthwt, fill = sex)) +
  geom_violin()+
  ggforce::geom_sina(alpha= 0.5) 
pgensex
# a violin plot provides
# similar, but more information than a violin plot
gensex_boxplot <- sheep %>%
  drop_na() %>%
  ggplot(aes(x = gen, y = birthwt, fill = sex)) +
  geom_boxplot()+
  ggforce::geom_sina(alpha= 0.5) 

gensex_boxplot

# genotype vs birth weight separated by year
# gen:year interaction 
pgenyear <- pgen + facet_wrap(vars(year))
pgenyear
table(sheep$gen, sheep$year)

# genotype vs. birthweight separated by year and sex
# gen:year:sex interaction
pgensexyear <- pgensex + facet_wrap(vars(year))


pgensexyear <-
  ggplot() +
  geom_violin(data = sheep, aes(x = gen, y = birthwt, fill = sex))+
  ggforce::geom_sina(data = sheep, aes(x = gen, y = birthwt, fill = sex), alpha= 0.5) +
  facet_wrap(vars(year))

pgensexyear

# Plotting the data reveals some consideration for 
# modeling:
# look at pgensexyear. What is the problem?
# FILL IN

## view sample size for each geotype
# by year
table(sheep$gen, sheep$year)

# EXERCISE:----------
## Task: make the same set of plots with weanwt 
# instead of birth weight

# remove 1991
sheep_allyears <- sheep
sheep <- sheep %>% filter(year != "91")


## Basic lm model ----------
#(using unstandardized data):
## Full model, all interaction terms
model1 <- lm(birthwt ~  gen + sex + year + gen:sex + gen:year + sex:year + gen:sex:year, data = sheep)

summary(model1)

coef(model1)

# Evaluate the model
par(mfcol= c(2,2))
plot(model1)
par(mfcol= c(1,1))

## observed vs. fitted
plot(sheep$birthwt, predict(model1), xlab = "observed", ylab = "predicted")
abline(a=0, b=1)

# Extract group predictions from the model-----
?predict.lm
# make a dataframe with the groups:
groupdf <- sheep %>% group_by(gen, sex, year) %>%
  summarize()
head(groupdf)

groupdf$pred_birthwt <- predict(model1, newdata = groupdf,se.fit = FALSE)

## Plot the data with predictions ---------

pgroups <- ggplot() + 
  geom_point(data = groupdf, aes(x = gen, y = pred_birthwt, color = year)) +
  facet_wrap(vars(sex))
pgroups

## interpret group effects--------
# the emmeans package is useful for this
## all sets of estimated marginal means:

allsets <- emmeans(model1, ".")
alleffects <- contrast(allsets, "eff")
alleffects

genemm <- emmeans(model1, c("gen"))
genemm
geneffect <- contrast(genemm,method = "pairwise")
geneffect
