library(tidyverse)
library(fixest)
library(caret)
library(modelsummary)
library(grid)


data <- read.csv( 'https://osf.io/4ay9x/download', stringsAsFactors = TRUE)

# selecting occupation 230 - Education administrators
data <- data %>% filter(occ2012 == 230)

##### adding variables ####
data <- data %>% mutate(female=as.numeric(sex==2)) %>%
  mutate(w=earnwke/uhours) %>%
  mutate(agesq=age^2) %>% 
  mutate(unionmember=ifelse(unionmme=='Yes',1,0))

###### plots #####

ggplot(data, aes(w)) +
  geom_histogram(aes(color = female))

ggplot(data, aes(age, w)) +
  geom_point() +
  geom_smooth(method = 'loess', se = F) +
  labs(x = "Age (years)",y = "Hourly wage (US dollars)")

ggplot(data, aes(factor(female), w)) +
  geom_boxplot() +
  geom_smooth(method = 'loess', se = F) +
  labs(x = "Sex", y = "Hourly wage (US dollars)")

######## Linear regressions #######

model1 <- as.formula(w ~ age + agesq)
model2 <- as.formula(w ~ age + agesq + female)
model3 <- as.formula(w ~ age + agesq + female + grade92)
model4 <- as.formula(w ~ age + agesq + female + grade92 + unionmember + marital + ownchild)

# Running simple OLS
reg1 <- feols(model1, data=data, vcov = 'hetero')
reg2 <- feols(model2, data=data, vcov = 'hetero')
reg3 <- feols(model3, data=data, vcov = 'hetero')
reg4 <- feols(model4, data=data, vcov = 'hetero')

# evaluation of the models
fitstat_register("k", function(x){length( x$coefficients ) - 1}, "No. Variables")
etable( reg1 , reg2 , reg3 , reg4 , fitstat = c('bic','rmse','n','k') )




