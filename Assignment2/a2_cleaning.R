library(tidyverse)
library(caret)
library(skimr)
library(grid)
library(glmnet)
library(cowplot)
library(modelsummary)
library(fixest)

data <- read_csv('listings_sd.csv')
drops <- c('id', 'listing_url', 'scrape_id', 'name', 'description',
           'neighborhood_overview', 'picture_url', 'host_id', 'host_url', 'host_name',
           'host_since', 'host_location', 'host_about', 'host_response_time',
           'host_thumbnail_url', 'host_picture_url',
           'host_neighbourhood', 'host_verifications',
           'neighbourhood_group_cleansed', 'latitude',
           'longitude', 'bathrooms', 'calendar_updated', 
           'calendar_last_scraped', 'minimum_minimum_nights', 'maximum_minimum_nights',
           'minimum_maximum_nights', 'maximum_maximum_nights', 'license')
data<-data[ , !(names(data) %in% drops)]

#display the class and type of each columns
sapply(data, class)
sapply(data, typeof)

#remove percentage signs
for (perc in c("host_acceptance_rate", "host_response_rate")){
  data[[perc]]<-gsub("%","",as.character(data[[perc]]))
}

#remove dollar signs from price variables
for (pricevars in c("price")){
  data[[pricevars]]<-gsub("\\$","",as.character(data[[pricevars]]))
  data[[pricevars]]<-gsub("\\,","",as.character(data[[pricevars]]))
  data[[pricevars]]<-as.numeric(as.character(data[[pricevars]]))
}

#format binary variables
for (binary in c("host_is_superhost","host_has_profile_pic","host_identity_verified",
                 "instant_bookable")){
  data[[binary]][data[[binary]]=="f"] <- 0
  data[[binary]][data[[binary]]=="t"] <- 1
}

#amenities
data$amenities<-gsub("\\{","",data$amenities)
data$amenities<-gsub("\\}","",data$amenities)
data$amenities<-gsub('\\"',"",data$amenities)
data$amenities<-as.list(strsplit(data$amenities, ","))

#define levels and dummies 
levs <- levels(factor(unlist(data$amenities)))
data<-cbind(data,as.data.frame(do.call(rbind, lapply(lapply(data$amenities, factor, levs), table))))

drops <- c("amenities","translation missing: en.hosting_amenity_49",
           "translation missing: en.hosting_amenity_50")
data<-data[ , !(names(data) %in% drops)]

names(data) <- trimws(names(data))
names(data) <- gsub(" ", "_", names(data))

###### selecting amenities w at least 500 ######
asd <- (colSums(data[, 44:2086]))
asd <- data.frame(asd)
asd500 <- asd %>% filter(asd > 500) %>% arrange(desc(asd))
amenities <- rownames(asd500)

drop <- c(' Essentials', ' Dishes and silverware', ' Essentials]', ' Wifi',
          ' Smoke alarm', ' Kitchen', ' Hangers', ' Shower gel', ' Outdoor furniture',
          ' Wine glasses', ' Single level home', ' Toaster', ' Lockbox', 
          ' Cleaning products', ' Keypad', ' Conditioner', ' Baking sheet', 
          ' Pack \\u2019n play/Travel crib', ' Outdoor dining area', ' Hot water kettle',
          ' Body soap', ' Portable fans', ' Lock on bedroom door', ' High chair',
          ' Keurig coffee machine', ' Fire pit', ' Clothing storage', ' Crib',
          ' Body soap]', ' Children\\u2019s dinnerware', ' Clothing storage: closet')

drop <- trimws(drop)
drop <- gsub(" ", "_", drop)

amenities <- amenities[!(amenities %in% drop)]

# separate df w only amenities
data_amen <- data[, 44:2086]
data_amen <-data_amen[ , (names(data_amen) %in% amenities)]

# merging variables for same feature
data_amen$carbon_monoxide_alarm <- data_amen$Carbon_monoxide_alarm + data_amen$`[Carbon_monoxide_alarm`

data_amen$cable_tv <- data_amen$Cable_TV + data_amen$TV_with_standard_cable
data_amen$cable_tv[data_amen$cable_tv == 2] <- 1 # replace 2 w/ 1

data_amen$hair_dryer <- data_amen$Hair_dryer + data_amen$`[Hair_dryer`

data_amen$long_term_stays_allowed <-  data_amen$Long_term_stays_allowed + data_amen$`Long_term_stays_allowed]`

data_amen$dryer <- data_amen$Dryer + data_amen$`Free_dryer_\\u2013_In_unit`

data_amen$patio_or_balcony <- data_amen$Patio_or_balcony + data_amen$Private_patio_or_balcony

#deleting old variables
data_amen <- data_amen %>% select(-c(Carbon_monoxide_alarm, `[Carbon_monoxide_alarm`,
                        Cable_TV, TV_with_standard_cable, Hair_dryer,
                        `[Hair_dryer`, Long_term_stays_allowed,
                        `Long_term_stays_allowed]`, Dryer, `Free_dryer_\\u2013_In_unit`,
                        Patio_or_balcony, Private_patio_or_balcony, Private_entrance))

data1 <- data[1:42]

data_final <- cbind(data1, data_amen)

write.csv(data_final,file="airbnb_sandiego_cleaned.csv")





