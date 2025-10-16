# this script uses objects, functions, and libraries defined or loaded in sampleModularScript1. 
# Be sure to restart your R session and clear your workspace before you begin.

# Run setup steps --------------
source("Rmodule2/sampleModularScript1.R") 
# The script's location is relative to the working directory (by default, the project directory)

# After running line 4, your Global Environment should contain all objects, functions, and libraries defined
# or loaded in that script. 

# Plot random vector -------------
## random_vec was defined in the setup script.
## base R
plot(random_vec, ylab = "Value", xlab = "Sample order")

## ggplot2 (part of the tidyverse package loaded in the setup script)
random_df <- data.frame(i = 1:length(random_vec),
                        random_int = random_vec)
ggplot(random_df) + geom_point(aes(x = i, y = random_int)) +
  labs(x = "Sample order", y = "Value")


# Inspect a loaded dataset----------
head(sheep)
