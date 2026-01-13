# Sheep model part 2-------------

# Research question: How does lamb genotype affect birth weight?

# This exercise adapts the basic model built in part 1 
# to a Bayesian model using brms

# load libraries --------
library(agridat)
#library(lme4)
library(tidyverse)
library(emmeans)
library(brms)
library(posterior)
library(bayesplot)

# load and set up data ----------
data("ilri.sheep")
sheep <- ilri.sheep
## year should be a category.
sheep$year <- factor(sheep$year)
# remove 1991
sheep <- sheep %>% filter(year != "91")
# standardize continuous variables
sheep <- sheep %>% 
  mutate(std_birthwt = scale(birthwt, center = TRUE, scale=TRUE)[,1],
         std_weanwt = scale(weanwt, center = TRUE, scale=TRUE)[,1])


# define model formula------
?brmsformula # documentation--lots of info

# 
# model1 <- lm(birthwt ~  gen + sex + year + 
#                gen:sex + gen:year + sex:year + 
#                gen:sex:year, data = sheep)
library(lme4)
model1 <- lmer(birthwt ~  gen + sex  + 
                gen:sex+ (1|year) , data = sheep)
summary(model1)

bf <- brmsformula(std_birthwt ~ gen + sex + gen:sex + (1|year) )

# set priors-----
?get_prior
?set_prior ## gives some examples

## check which parameters can have priors
brms::default_prior(bf, data = sheep)

# define priors 
bprior <- c(prior(normal(0,3), class = "b"),
            prior(normal(0,3), class = "b",coef = "genDR"),
            prior(normal(0,3), class = "b",coef = "genRD"),
            prior(normal(0,3), class = "b",coef = "genRR"),
            prior(normal(0,3), class = "b",coef = "sexM"),
            prior(normal(0,3), class = "b",coef = "genDR:sexM"),
            prior(normal(0,3), class = "b",coef = "genRD:sexM"),
            prior(normal(0,3), class = "b",coef = "genRR:sexM"),
            #tried df = 2.5
            prior(student_t(3, 0, 5), class = "sd",group = "year", lb = 0),
            prior(student_t(3, 0, 2.5), class = "sigma", lb = 0))

# rules of thumb for setting the sd for a normal prior:
# figure out the scale of the phenomenon, multiply by 3 

bprior

curve(dnorm(x, mean = 0, sd = 3), from = -10, 10)
#student t: df drives the heaviness of the tail. 
curve(dstudent_t(x, mu = 0, sigma = 3, df=1000), from = 0, 10, col="red")
curve(dstudent_t(x, mu = 0, sigma = 3, df = 2.5), from = 0, 10, add =TRUE)


# Prior predictive check
## one way to evaluate priors is by predicting parameters using only data and no priors
?bf
?brm
bf
priorcheck <- brm(bf, data = sheep, prior = bprior, 
                  sample_prior = "only",
                  chains = 4, 
                  iter = 1000, 
                  warmup = 500,
                  cores = 4)
summary(priorcheck)
priordraws <- posterior::as_draws_matrix(priorcheck)
priordraws[,"b_Intercept"]
dim(priordraws)

priorpred <- posterior_predict(priorcheck,ndraws = 100)
dim(priorpred)
priorpredsum <- summarize_draws(priorpred)
priorpredsum
sheepprior <- bind_cols(sheep,priorpredsum)
summary(priorcheck)
summary(sheep$std_birthwt)
plot(sheepprior$std_birthwt, sheepprior$median)



## adjust priors if needed

# fit model --------
modelfit <- brm(bf, data = sheep, prior = bprior, 
                  sample_prior = "no",
                  chains = 4,
                  iter = 1000,
                  warmup = 500 ,
                  cores = 4)
# this takes a while

# the model in stan code:
modelfit$model


## inspect model-------
# trace plots
##extract draws:
pdraws <- posterior::as_draws_df(modelfit)
## you can draw trace plots like this:
plot(pdraws$b_Intercept)

## plot of slopes:
bayesplot::mcmc_trace(pdraws, pars_regex = "b")

# plot of sigmas
mcmc_trace(pdraws, pars = c("sd_year_Intercept","sigma"))

# histograms:
mcmc_hist(pdraws) # draws all at once if you don't sepcify pars


# This shows trace plots and density plots 
# for all parameters
plot(modelfit)


# posterior predictive check
## blue lines are the data from 10 draws
# on the posterior distribution
# black line is the observed data
pp_check(modelfit)

# Empirical coverage probability--------
## sample from the posterior distribution
?posterior_predict
?summarize_draws

predsample <-posterior_predict(modelfit,ndraws = 1000) 
preddf <- summarize_draws(predsample) 
sheep2 <- bind_cols(sheep, preddf) %>%
  mutate(index = rank(birthwt))
head(preddf)
ggplot(sheep2) + geom_segment(aes(x = index,xend = index,
                                  y = q5, yend = q95)) +
  geom_point(aes(x = index, y = std_birthwt))

# Calculate empirical coverage probability:
ecp <- sum(sheep2$std_birthwt >= sheep2$q5 &
             sheep2$std_birthwt <= sheep2$q95)/nrow(sheep2)

ecp # 91% 


# Extract model results --------
genmeans <- emmeans(modelfit , c("gen"))
genmeans
gencontrasts <- contrast(genmeans,method = "pairwise")
gencontrasts

# compare results to frequentist model---------
freqmod <- lmer(std_birthwt ~ gen + sex + gen:sex + (1|year), data = sheep)

genmeansf <- emmeans(freqmod, "gen")
genmeans

contrast(genmeansf,method = "pairwise")
contrast(genmeans, method = "pairwise")

# predict data on the posterior distribution-----
## samples with the same structure as the original data
psamp <- posterior_predict(modelfit,ndraws = 1000)

## predict data with a different structure:
newdata <- data.frame(gen = c("DD","RR"),
                      sex = c("male","male"),
                      year = c("96") )

psampnew <- posterior_predict(modelfit,ndraws = 1000,newdata = newd)

## predict data excluding the group-level effect of year
psampnoyear <- posterior_predict(modelfit,ndraws = 1000,re_formula = ~0)
