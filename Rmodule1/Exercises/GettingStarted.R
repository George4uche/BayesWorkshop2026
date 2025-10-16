# This is an R script, where we can save our code, 
# run code line-by-line, or run the entire script

# Text preceded by # is a comment

# Our first step is to install required packages (Only do this once!)
# For our exercises, we want to install two packages: agridat and tidyverse 
# Type the two commented commands below into your console and hit enter
install.packages("agridat")
install.packages("tidyverse")

# load required packages
library(agridat)
library(tidyverse)

# to learn about these packages, use ?
# You can use ? to learn about any packages or functions within packages
?agridat
?tidyverse

# we are going to use a package called agridat 
# and a dataset within that package called ilri.sheep

# to read in data from a package
data(ilri.sheep)

# to view the dataset, we use the View command
View(ilri.sheep)

# to learn more about the dataset, we can use ? 
?ilri.sheep

# One quick way to get an idea about a dataset is to use the summary command 
summary(ilri.sheep)

# What did you learn from using summary? 
# What else do you want to know about the data? 

