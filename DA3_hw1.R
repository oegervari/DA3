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
  geom_histogram(aes(color = female)) +
  labs(x = "Hourly wage (US dollars)")

ggplot(data, aes(age, w)) +
  geom_point() +
  geom_smooth(method = 'loess', se = F) +
  labs(x = "Age (years)",y = "Hourly wage (US dollars)")

ggplot(data, aes(factor(female), w)) +
  geom_boxplot() +
  geom_smooth(method = 'loess', se = F) +
  labs(x = "Sex", y = "Hourly wage (US dollars)")

# Lowess vs. quadratic specification with age
ggplot(data = data , aes(x=age,y=w)) +
  geom_smooth( aes(colour='red'), method="loess", formula = y ~ x,se=F, size=1) +
  geom_smooth( aes(colour='black'), method="lm", formula = y ~ poly(x,2) , se=F, size=1) +
  geom_point( aes( y = w ) , color = 'blue', size = 1,  shape = 16, alpha = 0.8, show.legend=F, na.rm = TRUE) + 
  labs(x = "Age (years)",y = "Price (US dollars)") +
  scale_color_manual(name="", values=c('red','black'),labels=c("Lowess in age","Quadratic in age")) +
  theme_bw()

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


####### Cross-validation ########

# Setting number of fold
k <- 5

set.seed(43502)
cv1 <- train(model1, data, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(43502)
cv2 <- train(model2, data, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(43502)
cv3 <- train(model3, data, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(43502)
cv4 <- train(model4, data, method = "lm", trControl = trainControl(method = "cv", number = k))

# Calculating RMSE for each fold and the average RMSE as well
cv <- c("cv1", "cv2", "cv3", "cv4")
rmse_cv <- c()

for(i in 1:length(cv)){
  rmse_cv[i] <- sqrt((get(cv[i])$resample[[1]][1]^2 +
                        get(cv[i])$resample[[1]][2]^2 +
                        get(cv[i])$resample[[1]][3]^2 +
                        get(cv[i])$resample[[1]][4]^2)/4)
}

# summarize results
cv_mat <- data.frame(rbind(cv1$resample[4], "Average"),
                     rbind(cv1$resample[1], rmse_cv[1]),
                     rbind(cv2$resample[1], rmse_cv[2]),
                     rbind(cv3$resample[1], rmse_cv[3]),
                     rbind(cv4$resample[1], rmse_cv[4])
)

colnames(cv_mat)<-c("Resample","Model1", "Model2", "Model3", "Model4")
cv_mat 

# Show model complexity and out-of-sample RMSE performance
m_comp <- c()
models <- c("reg1", "reg2", "reg3", "reg4")
for( i in 1 : length(cv) ){
  m_comp[ i ] <- length( get( models[i] )$coefficient  - 1 ) 
}

m_comp <- tibble( model = models , 
                  complexity = m_comp,
                  RMSE = rmse_cv )

ggplot( m_comp , aes( x = complexity , y = RMSE ) ) +
  geom_point(color='red',size=2) +
  geom_line(color='blue',size=0.5)+
  labs(x='Number of explanatory variables',y='Averaged RMSE on test samples',
       title='Prediction performance and model compexity') +
  theme_bw()





