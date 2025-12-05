---
output:
  word_document: default
  html_document: default
---
# Set up instructions Part II: Packages for Bayesian data analysis

## Overview
Bayesian data analysis relies on computers to sample from statistical distributions. There are various open-source and commercial computer software options available. For this course, we will be using an open-source tool called **Stan** (https://mc-stan.org/). Stan has a lively community of developers and contributers and tools for multiple programing environments, including R.

Stan is a *programming language* for Bayesian data analysis that is compiled in C++. To use Stan within R, you need an interpreter that can compile Stan code into C++ from an R environment. The two most common interpreters are **Rstan** and **CmdStanR**. For this workshop, we will be using **CmdStanR**. **CmdStanR** is an interface for an external software called **CmdStan**, which runs Stan Code (https://mc-stan.org/cmdstanr/articles/cmdstanr.html#introduction). **Rstan** is available from CRAN as the package *rstan* (https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started). Both interpreters are fine, but CmdStanR is more computationally efficient and is better synced to updates in the Stan language (see https://mc-stan.org/cmdstanr/articles/cmdstanr.html#comparison-with-rstan). However, because Rstan is the oldest Stan interpreter for R, it comes up in a lot of Bayesian statistics learning resources and so it can be helpful to have it installed.

For this workshop, we will be using a package called *brms* that translates an R-friendly syntax into Stan code (https://cran.r-project.org/web/packages/brms/index.html). You can run your own Stan code through R, but *brms* makes it a little easier to get up and running. *brms* uses a syntax for multi-level regression models that is similar to that in the frequentist packages *lme4* and *nmle*. This syntax makes it relatively easy to code complex, multi-level models that would be difficult to implement in Stan. However, **it is helpful to familiarize yourself with the basic structure of a Stan model** for several reasons. The main reason is that Stan code tells you everything about how the model is structured and computed. Every component and variable is explicit and the syntax is human-readable. The *brms* package hides a lot of these details "under the hood", so to speak. I find it helpful to read the Stan code created by *brms* to understand what my model is doing and how *brms* interprets my input. This becomes more valuable when your model is not working well and you need to troubleshoot parameters. You can learn more about Stan at https://mc-stan.org/docs/reference-manual/ and   https://mc-stan.org/docs/2_20/stan-users-guide-2_20.pdf. 



Here is a diagram to help explain how the parts fit together:

![Bayesian Computation Ecosystem](setupimages/ComputationalEcosystemForBayesianAnalysis.drawio.svg)


## Necessary packages

- *tidyverse*:
	This is a bundle of packages with useful data science tools. 

- *remotes*:
	This package allows you to install packages from remote servers, such as GitHub. 
	
- *brms*:
	This is a package used for Bayesian data analysis. Installing *brms* will also install a lot of other packages related to Bayesian data analysis, including *rstan*.  

- *cmdstanr* :
	This package installs and interacts with CmdStan.

## Optional packages.

- *rethinking*: This package corresponds to the course and book by Richard McElreath (https://github.com/rmcelreath/rethinking/). It has some model fitting tools that are similar to the Stan syntax, in that they show the model explicitly in a way that is easier to connect to the theory.  

## Installation.

Open R studio and run these lines of code to install packages. You only need to do this once!

```
install.packages("tidyverse") 
# this installs a suite of packages. 
# If it asks you to install from a binary source, just click "yes". 

install.packages("remotes")

install.packages( "remotes")
remotes::install_github("stan-dev/cmdstanr")

install_cmdstan() # this installs CmdStan on your computer.

install.packages("brms") #this will also install dependences

#OPTIONAL:
remotes::install_github("rmcelreath/rethinking")
```


