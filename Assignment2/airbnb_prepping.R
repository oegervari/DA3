library(tidyverse)
library(caret)
library(skimr)
library(grid)
library(glmnet)
library(cowplot)
library(modelsummary)
library(fixest)

rm(list=ls())

data <- read_csv("/Users/oszkaregervari/Documents/CEU/Winter_term/DA_3/DA3/assignment 2/airbnb_sandiego_cleaned.csv") %>% 
  mutate_if(is.character, factor)
glimpse(data)

######### feature engineering ####

#counting NAs
to_filter <- sapply(data, function(x) sum(is.na(x)))
data.frame(to_filter[to_filter > 0]) %>% arrange(desc(to_filter.to_filter...0.))


# bedrooms -> too many NA
# beds is ok
# bathroom text is ok

pricee <- data %>% count(price)

ggplot(pricee, aes(x=price)) + 
  geom_histogram(binwidth = 20) + 
  theme_bw()

summary(data$price)

# dropping listing where price = 0 USD
data <- data %>% filter(!(price == 0))
# dropping listings where price is 2500+ USD
# data <- data %>% filter(price <= 2500)


# dropping listings w/o reviews (review_scores_value is NA)

# data <- data %>% filter(!is.na(review_scores_value))

#counting NAs
to_filter <- sapply(data, function(x) sum(is.na(x)))
data.frame(to_filter[to_filter > 0]) %>% arrange(desc(to_filter.to_filter...0.))

#Room type as factor
datasummary(room_type ~ N + Percent() , data = data )
data <- data %>%
  mutate(f_room_type = factor(room_type))

# Rename room type because it is too long
data$f_room_type2 <- factor(ifelse(data$f_room_type== "Entire home/apt", "Entire/Apt",
                                   ifelse(data$f_room_type== "Private room", "Private",
                                          ifelse(data$f_room_type== "Shared room", "Shared", "."))))
data <- data %>%
  mutate(
    f_property_type = factor(property_type))

#data %>% count(room_type2)

# transforming bathroom_text to num

data <- separate(data, bathrooms_text, ' ', into = c('bathroom', 'garbage'))
data <- data %>% select(-c(garbage))
data$bathroom <- as.numeric(data$bathroom)



# counting NAs
to_filter <- sapply(data, function(x) sum(is.na(x)))
data.frame(to_filter[to_filter > 0]) %>% arrange(desc(to_filter.to_filter...0.))

##### add new numeric columns from certain columns ####
numericals <- c("accommodates","bathroom", "beds", "bedrooms",
                "minimum_nights", "maximum_nights",
                "review_scores_rating", "review_scores_cleanliness",
                "number_of_reviews")
data <- data %>%
  mutate_at(vars(all_of(numericals)), lst("n"=as.numeric))


# rename columns so they start with n_ as opposed to end with _n
nnames <- data %>%
  select(ends_with("_n")) %>%
  names()
nnames_i <- match(nnames, colnames(data))
colnames(data)[nnames_i] <- paste0("n_", numericals)

##### create days since first review #####
data <- data %>%
  mutate(
    n_days_since = as.numeric(as.Date(last_scraped,format="%Y-%m-%d") -
                                as.Date(first_review ,format="%Y-%m-%d")))

##### create dummy vars #####
dummies <- names(data %>% select(c(5, 8, 9, 40, 44:99)))

data <- data %>%
  mutate_at(vars(dummies), funs("d"= (.)))
# rename columns
dnames <- data %>%
  select(ends_with("_d")) %>%
  names()
dnames_i <- match(dnames, colnames(data))
colnames(data)[dnames_i] <- paste0("d_", tolower(gsub("[^[:alnum:]_]", "",dummies)))

# keep columns if contain d_, n_,f_, p_, usd_ and some others
data <- data %>%
  select(matches("^d_.*|^n_.*|^f_.*"), price,
         neighbourhood_cleansed, room_type, property_type)

#####################
### look at price ###
#####################
datasummary( price ~ Mean + Median + Min + Max + P25 + P75 , data = data )
data <- data %>%
  mutate(ln_price = log(price))
# data1 <- data %>%
#   filter(price <1000)


######### adding features #######

# Squares and further values to create
data <- data %>%
  mutate(n_accommodates2=n_accommodates^2, 
         ln_accommodates=log(n_accommodates) ,
         ln_accommodates2=log(n_accommodates)^2,
         ln_beds = log(n_beds),
         ln_number_of_reviews = log(n_number_of_reviews+1)
  )

# Pool accomodations with 0,1,2,10 bathrooms
data <- data %>%
  mutate(f_bathroom = cut(n_bathroom, c(0,1,2,10), labels=c(0,1,2), right = F) )

# Pool num of reviews to 3 categories: none, 1-51 and >51
data <- data %>%
  mutate(f_number_of_reviews = cut(n_number_of_reviews, c(0,1,51,max(data$n_number_of_reviews)), labels=c(0,1,2), right = F))

# Pool and categorize the number of minimum nights: 1,2,3, 3+
data <- data %>%
  mutate(f_minimum_nights= cut(n_minimum_nights, c(1,2,3,max(data$n_minimum_nights)), labels=c(1,2,3), right = F))

# Change Infinite values with NaNs
for (j in 1:ncol(data) ) data.table::set(data, which(is.infinite(data[[j]])), j, NA)

##### missing values ########

# where do we have missing variables now?
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

# imputing 

data <- data %>%
  mutate(
    n_bathroom =  ifelse(is.na(n_bathroom), median(n_bathroom, na.rm = T), n_bathroom), #assume at least 1 bath
    n_beds = ifelse(is.na(n_beds), n_accommodates, n_beds), #assume n_beds=n_accomodates
    f_bathroom=ifelse(is.na(f_bathroom),1, f_bathroom),
    n_bedrooms=ifelse(is.na(n_bedrooms),1, n_bedrooms), # assuming there is one common area for sleeping
    d_host_is_superhost = ifelse(is.na(d_host_is_superhost), 0, d_host_is_superhost), # assume they are not superhosts
    f_number_of_reviews=ifelse(is.na(f_number_of_reviews),1, f_number_of_reviews),
    ln_beds=ifelse(is.na(ln_beds),0, ln_beds)
  )

to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

# replacing missing variables w/ zeros and adding flags

data <- data %>%
  mutate(
    flag_days_since=ifelse(is.na(n_days_since),1, 0),
    n_days_since =  ifelse(is.na(n_days_since), median(n_days_since, na.rm = T), n_days_since),
    flag_review_scores_rating=ifelse(is.na(n_review_scores_rating),1, 0),
    n_review_scores_rating =  ifelse(is.na(n_review_scores_rating), median(n_review_scores_rating, na.rm = T), n_review_scores_rating),
    flag_review_scores_cleanliness=ifelse(is.na(n_review_scores_cleanliness),1, 0),
    n_review_scores_cleanliness =  ifelse(is.na(n_review_scores_cleanliness), median(n_review_scores_cleanliness, na.rm = T), n_review_scores_cleanliness),
    flag_n_number_of_reviews=ifelse(n_number_of_reviews==0,1, 0)
  )
table(data$flag_days_since)

# redo features
# Create variables, measuring the time since: squared, cubic, logs
data <- data %>%
  mutate(
    ln_days_since = log(n_days_since+1),
    ln_days_since2 = log(n_days_since+1)^2,
    ln_days_since3 = log(n_days_since+1)^3 ,
    n_days_since2=n_days_since^2,
    n_days_since3=n_days_since^3,
    ln_review_scores_rating = log(n_review_scores_rating),
    ln_days_since=ifelse(is.na(ln_days_since),0, ln_days_since),
    ln_days_since2=ifelse(is.na(ln_days_since2),0, ln_days_since2),
    ln_days_since3=ifelse(is.na(ln_days_since3),0, ln_days_since3),
  )

# Look at data
datasummary( f_room_type2 ~ N , data = data )
datasummary_skim( data , 'categorical' )

# NAs

to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

ggplot(data, aes(n_days_since2, price)) +
  geom_point() +
  geom_smooth(method = 'loess')

data_amen$b <- data_amen$Patio_or_balcony + data_amen$Private_patio_or_balcony

data$d_beachfront <-  data$d_beachfront + data$d_waterfront
data <- data %>% select(-c(d_waterfront))

data$d_beachfront[data$d_beachfront == 2] <- 1 # replace 2 w/ 1

##### Saving data ######
write_csv(data, "airbnb_sandiego_prepped.csv")


###### filtering out non-relevant observations #########

# price between 0 and 2500 usd (done alread)

# size - 2 < accomodation < 7

ggplot(data, aes(x = accommodates)) +
  geom_histogram(bins = 30) +
  theme_bw()

summary(data$accommodates)

data <- data %>%
  filter(accommodates < 7) %>% filter(accommodates > 1)

skimr::skim(data)

##### property type #########

property <- data %>% count(property_type, sort = T)
# 
# data <- data %>% 
#   mutate(
#   property_type2 = ifelse(grepl("Entire.*", data$property_type) == T, 'Entire', 'other'),
#   property_type2 = ifelse(grepl("Private.*", data$property_type), 'Private', property_type2),
#   property_type2 = ifelse(grepl("Shared.*", data$property_type), 'Shared', property_type2),
#   property_type2 = ifelse(grepl("*.(hotel|hostel).*", data$property_type), 'Hostel/Hotel', property_type2)
# )

# data %>% count(property_type2, sort = T)

data %>% count(room_type, sort = T)

# keeping only apartments
keep <- c('Entire rental unit', 'Entire condominium (condo)', 'Entire loft', 
          'Entire serviced apartment', 'Entire guest suite')


data <- data %>% filter(property_type %in% keep)




######### Descriptive statistics #####

# Average price by `property_type`, `room_type`
datasummary( room_type*price + price ~ Mean + SD + P25 + P75 + N, data = data )

# NB all graphs, we exclude  extreme values of price for plotting
datau <- subset(data, price<600)

###
# Graphs
# price
ggplot(data=datau, aes(x=price)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 10, boundary=0,
                 fill = 'navyblue', color = 'white', size = 0.25, alpha = 0.8,  show.legend=F,  na.rm=TRUE) +
  labs(x = "Price (US dollars)",y = "Percent")+
   scale_y_continuous(expand = c(0.00,0.00),limits=c(0, 0.06), breaks = seq(0, 0.06, by = 0.03), labels = scales::percent_format(1)) +
  theme_minimal() 


# lnprice
ggplot(data=datau, aes(x=ln_price)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 0.15,
                 color = 'white', fill = 'navyblue', size = 0.25, alpha = 0.8,  show.legend=F,  na.rm=TRUE) +
  coord_cartesian(xlim = c(2.5, 6.5)) +
  scale_y_continuous(expand = c(0.00,0.00),limits=c(0, 0.12), breaks = seq(0, 0.12, by = 0.04), labels = scales::percent_format(1L)) +
  scale_x_continuous(expand = c(0.00,0.01),breaks = seq(2.4,6.6, 0.6)) +
  labs(x = "ln(price, US dollars)",y = "Percent")+
  theme_minimal() 



# Boxplot of price by number of persons
ggplot(datau, aes(x = factor(accommodates), y = price,
                  fill = factor(room_type), color=factor(room_type))) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
  stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
  labs(x = "Accomodates (Persons)",y = "Price (US dollars)")+
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 600), breaks = seq(0,600, 50))+
  theme_bw() +
  theme(legend.position = c(0.3,0.8)        )


########## setting up models #########













