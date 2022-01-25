library(tidyverse)
library(fixest)
library(caret)
library(modelsummary)
library(grid)


data <- read.csv( 'https://osf.io/4ay9x/download', stringsAsFactors = TRUE)

data <- data %>% mutate(female=as.numeric(sex==2)) %>%
  mutate(w=earnwke/uhours) %>%
  mutate(agesq=age^2) %>% 
  mutate(unionmember=ifelse(unionmme=='Yes',1,0))









