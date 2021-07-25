# install.packages("remotes")
# remotes::install_github("kevinykuo/insurance")

library(insurance)
lapse_study

lapse_data <- lapse_study

library(tidyverse)
library(ggplot2)

ggplot(data = lapse_study, aes(x = as.factor(duration), y = lapse_count)) + # takes so long to plot
  geom_boxplot()

trim_lapse <- head(lapse_data,20)
trim_lapse

ggplot(data = trim_lapse, aes(x = as.factor(duration), y = lapse_count)) + # very quick plot. 
  geom_boxplot()