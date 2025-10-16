# This script runs some setup steps that are used by another script. 

# Load libraries ------------
## (the ----- formats the comment as a header) in the R document outline,
## which can be useful for navigating your code 
library(tidyverse)
library(agridat)


# Create a vector of random integers----------------

min_integer <- 1 # minimum of the range
max_integer <- 100 # maximum of the range
nsamp <- 100 # length of random vector

random_vec <- sample(min_integer:max_integer, size = nsamp, replace = TRUE)

# Bonus practice: How would you convert this code into a function?

# Define a function:-----------
standard_error <- function(x){sd(x)/length(x)}

# define a dataset ---------
data(ilri.sheep)
sheep <- ilri.sheep
rm(ilri.sheep)