library(data.table)
library(dplyr)
library(tidyverse)

#########CALENDAR DATATSET #####################

calendar <- fread('calendar.csv.gz', na.strings = "NA")

#get summary and structure of the calendar dataset
summary(calendar)
str(calendar)

#look at the first few rows
head(calendar)

#check to see how many potential values "available" feature can take on
unique(calendar$available)

#convert price and adjusted price to integer
calendar$price <- as.numeric(gsub("\\$","",calendar$price))
calendar$adjusted_price <- as.numeric(gsub("\\$","",calendar$adjusted_price))

#investigating missing values
mean(is.na(calendar$price)) #<0.5%
mean(is.na(calendar$adjusted_price)) #<0.5%
mean(is.na(calendar$minimum_nights)) #<0.01%
mean(is.na(calendar$maximum_nights)) #<0.01%

#drop adjusted price
calendar = subset(calendar, select = -c(adjusted_price))


############# LISTINGS DATASET ################

#read in listings dataset
listings <- fread('listings.csv.gz', na.strings="NA")

summary(listings)
str(listings)

#columns to remove
#listing url, scrape_id, last_scraped, picture_url, host_url, host_name, host_location, host_about,
# neighborhood, host_thumbnail_url, host_picture_url, neighbourhood_group_cleansed, bathrooms, 
# calendar_updated, license

col_drop_list <- c('listing url', 'scrape_id', 'last_scraped', 'picture_url', 'host_url', 'host_name', 
   'host_location', 'host_about', 'neighborhood', 'host_thumbnail_url', 'host_picture_url', 
   'neighbourhood_group_cleansed', 'bathrooms', 'calendar_updated', 'license')

listings_drop <- listings[, !c('listing_url', 'scrape_id', 'last_scraped', 'picture_url', 'host_url', 'host_name', 
                               'host_location', 'host_about', 'neighbourhood', 'host_thumbnail_url', 'host_picture_url', 
                               'neighbourhood_group_cleansed', 'bathrooms', 'calendar_updated', 'license', 'thumbnail_url',
                               'medium_url', 'xl_picture_url', 'host_acceptance_rate', 'first_review', 'last_review')]
#determine which columns have missing values
colnames(listings_drop)[colSums(is.na(listings_drop)) > 0]
summary(listings_drop$reviews_per_month)

#columns to drop NAs, only missing 1 observation - host_since, host_listings_count, host_total_listings_count 
listings_drop <- na.omit(listings_drop, cols = c("host_since", "host_listings_count", "host_total_listings_count"))
#determine percentage of missing values in dataset
mean(is.na(listings_drop))


str(listings_drop)
#deal with columns that are in wrong data type
#percentage columns are strings of percentages
listings_drop$host_response_rate <- as.numeric(sub("%",'',listings_drop$host_response_rate))/100
listings_drop$host_acceptance_rate <- as.numeric(sub("%",'',listings_drop$host_acceptance_rate))/100
#price is a character type, should be numeric. strip dollar sign and convert to numeric
listings_drop$price <- as.numeric(gsub("\\$","",listings_drop$price))

str(listings_drop)


############ REVIEWS DATASET ###################

reviews <- fread('reviews_dec.csv.gz')

str(reviews)
summary(reviews)

#get reviews for 2020 only
reviews <- reviews[date >= "2020-01-01" & date <= "2020-12-31"]
#reviews data looks ok. Few variables to start with and all in correct format/no missing values


##### ISOLATE COVID DATA RANGE TO ONLY INCLUDE 2020 #####
covid_calendar <- calendar[date >= "2020-01-01" & date <= "2020-12-31"]

### Dealing with Missing Values ##
str(covid_calendar)
summary(covid_calendar)
# Calendar -- price, min/max nights have missing values -- impute mean value
covid_calendar$price[is.na(covid_calendar$price)] <- mean(covid_calendar$price, na.rm=TRUE)
covid_calendar$minimum_nights[is.na(covid_calendar$minimum_nights)] <- mean(covid_calendar$minimum_nights, na.rm=TRUE)
covid_calendar$maximum_nights[is.na(covid_calendar$maximum_nights)] <- mean(covid_calendar$maximum_nights, na.rm=TRUE)


summary(listings_drop)
colnames(listings_drop)[apply(listings_drop, 2, anyNA)]

#impute median for response rate, bedrooms, beds
listings_drop$bedrooms[is.na(listings_drop$bedrooms)] <- median(listings_drop$bedrooms, na.rm=TRUE)

#impute mean for square_feet, price, review_scores_####
listings_drop$reviews_per_month[is.na(listings_drop$reviews_per_month)] <- mean(listings_drop$reviews_per_month, na.rm=TRUE)

summary(listings_drop)

#reviews is clean, missing values wise

#save cleaned dataframes to csv
write.csv(covid_calendar, "calendar_clean.csv", row.names = FALSE)
write.csv(listings_drop, "listings_clean.csv", row.names = FALSE)
write.csv(reviews, "reviews_clean.csv", row.names = FALSE)




