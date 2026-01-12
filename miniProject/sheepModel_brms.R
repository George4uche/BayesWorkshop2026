# Sheep model part 2-------------

# Research question: How does lamb genotype affect birth weight?

# This exercise adapts the basic model built in part 1 
# to a Bayesian model using brms

# load libraries --------
library(agridat)
library(lme4)
library(tidyverse)
library(emmeans)
library(brms)
library(posterior)

# load and set up data ----------
data("ilri.sheep")
sheep <- ilri.sheep
## year should be a category.
sheep$year <- factor(sheep$year)
# standardize continuous variables
sheep <- sheep %>% 
  mutate(std_birthwt = scale(birthwt, center = TRUE, scale=TRUE)[,1],
         std_weanwt = scale(weanwt, center = TRUE, scale=TRUE)[,1])
# remove 1991
sheep <- sheep %>% filter(year != "91")


# define model formula------
?brmsformula # documentation--lots of info

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
            prior(student_t(3, 0, 2.5), class = "sd",group = "year", lb = 0),
            prior(student_t(3, 0, 2.5), class = "sigma", lb = 0))

bprior


# Prior predictive check
## one way to evaluate priors is by predicting parameters using only data and no priors
?bf
priorcheck <- brm(bf, data = sheep, prior = bprior, 
                  sample_prior = "only",
                  chains = 4, 
                  iter = 1000, 
                  warmup = 500 ,
                  cores = 4)
summary(priorcheck)
summary(sheep$std_birthwt)
priorcheck$model # the model in stan code

## adjust priors if needed

# fit model --------
modelfit <- brm(bf, data = sheep, prior = bprior, 
                  sample_prior = "no",
                  chains = 4,
                  iter = 1000,
                  warmup = 500 ,
                  cores = 4)
# this takes a while

## inspect model-------
# trace plots
# posterior densities
plot(modelfit)

# posterior predictive check
## blue lines are the data from 10 draws
# on the posterior distribution
# black line is the observed data
pp_check(modelfit)

# Extract model results --------
genmeans <- emmeans(modelfit , c("gen"))
gencontrasts <- contrast(genmeans,method = "pairwise")
genmeans
gencontrasts

# Empirical coverage probability--------
## sample from the posterior distribution
?posterior_predict
?summarize_draws
hpd <- 
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
