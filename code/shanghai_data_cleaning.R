# CLEAR MEMORY
rm(list=ls())
#import libraries
library(data.table)
library(tidyverse)
library(caret)
library(skimr)
library(grid)
library(glmnet)
library(cowplot)
library(modelsummary)
library(fixest)
library(Hmisc)
library(scales)
library(ggpubr)

########################################
#####

# Load data
gz <- gzfile('~/Desktop/BA-Courses/Data_Analysis3/Assignment2/data/raw/shanghai.csv.gz','rt') 
listings <- read.csv( gz, header = T )

# filter the data frame for apartments that can accommodate 2-6 guests --------
# keep apartments which can host 2-6 guests
data <- listings %>% filter( accommodates %in% c(2:6) )

# check property_type variable
datasummary(property_type ~ N + Percent(), data = data )

# keep if property_type suggests that the place is an apartment
data <- data %>% 
  filter( property_type %in% c('Entire loft', 'Entire serviced apartment','Entire rental unit') )


### filter variables which cannot be used for prediction in this task
data <- data %>% select( -c( listing_url, scrape_id, description, neighborhood_overview, 
                                   picture_url, host_url, host_name, host_location, host_about,
                                   host_thumbnail_url, host_picture_url, host_neighbourhood, 
                                   host_listings_count, neighbourhood_group_cleansed, bathrooms, 
                                  minimum_minimum_nights, minimum_maximum_nights, 
                                   minimum_nights_avg_ntm, maximum_minimum_nights, 
                                   maximum_maximum_nights, maximum_nights_avg_ntm, calendar_updated, 
                                   has_availability, availability_30, availability_60, availability_90, availability_365, 
                                   calendar_last_scraped, number_of_reviews_ltm, number_of_reviews_l30d, license ) )
glimpse(data)

### convert price variable to numeric
# remove $ sign and comma
data$price <- as.numeric(gsub("[\\$,]","",data$price))

names(data)[names(data) == "bathrooms_text"] <- "bathrooms"
# Remove text from bathrooms column
table(data$bathrooms)
data$bathrooms <- gsub("baths", "", data$bathrooms)
data$bathrooms <- gsub("bath", "", data$bathrooms)
data$bathrooms <- replace(data$bathrooms,data$bathrooms == 'Half-',0.5)
data$bathrooms <- as.numeric(data$bathrooms)

# replace 'NA' string with NA
data$bathrooms <- ifelse( data$bathrooms == 'NA', NA, as.numeric( data$bathrooms ) )

### convert host_response_rate variable to numeric
# replace 'N/A' string with NA
data$host_response_rate <- ifelse( data$host_response_rate == 'N/A', NA, data$host_response_rate )

# remove % sign and convert to numeric
data$host_response_rate <- as.numeric( gsub( "%","",data$host_response_rate ) )

### convert host_acceptance_rate variable to numeric
data$host_acceptance_rate <- ifelse( data$host_acceptance_rate == 'N/A', NA, data$host_acceptance_rate )

# remove % sign and convert to numeric
data$host_acceptance_rate <- as.numeric( gsub( "%","",data$host_acceptance_rate ) )

### extract amenities
# remove unnecessary signs and convert to list
data$amenities <- tolower( data$amenities )
data$amenities <- gsub("\\[","", data$amenities)
data$amenities <- gsub("\\]","", data$amenities)
data$amenities <- gsub('\\"',"",data$amenities)
data$amenities <- as.list(strsplit(data$amenities, ","))

# define levels and dummies and append to data
levels <- levels(factor(unlist(data$amenities)))
data <- cbind(data,as.data.frame(do.call(rbind, lapply(lapply(data$amenities, factor, levels), table))))


### convert date columns to date
data$first_review <- as.Date( data$first_review, format="%Y-%m-%d" )

data$last_review <- as.Date( data$last_review, format="%Y-%m-%d" )

data$host_since <- as.Date( data$host_since, format="%Y-%m-%d" )

# function to aggregate several columns of same type/category into one generic binary column
aggregate_columns <- function(word){
  
  # subset columns which contain a specific word and save them to another dataframe, also select 'id' to use for merge later
  new_df <- data %>% select(contains(word),"id")
  
  # go row by row to see if any of the rows have a 1, if it does, populate new column 'col_name' with 1
  new_df$col_name <- apply(new_df[0:ncol(new_df)], 1, function(x) ifelse(any(x == 1), '1', '0'))
  
  # save new column and id column to another dataframe, this new dataframe is used to merge with original dataframe
  new_df_merge <- new_df %>% select(id,col_name)
  
  # merge original dataframe and new_df_merge by 'id'
  data <- merge(data,new_df_merge,by = "id", all = FALSE)
  
  # remove the new column and 'id' column from the new_df dataframe
  new_df <- new_df %>% select(-c(id,col_name))
  
  # remove the selected columns from original dataframe since they have already been aggregated into a new column and merged
  data <<- data %>% select(-colnames(new_df))
}

# aggregate columns for a few amenities that could be important for predicting price
aggregate_columns("refrigerator")
data <- data %>% rename("refrigerator" = col_name)

aggregate_columns("tv")
data <- data %>% rename("tv" = col_name)

aggregate_columns("wifi")
data <- data %>% rename("wifi" = col_name)

aggregate_columns("baby")
data <- data %>% rename("baby" = col_name)

aggregate_columns("stove")
data <- data %>% rename("stove" = col_name)

aggregate_columns("air conditioning")
data <- data %>% rename("air_conditioning" = col_name)

aggregate_columns("microwave")
data <- data %>% rename("microwave" = col_name)

aggregate_columns("free parking")
data <- data %>% rename("free_parking" = col_name)

aggregate_columns("paid parking")
data <- data %>% rename("paid_parking" = col_name)

# drop the amenities column because a csv cannot store it since it is a list
data <- data %>% select( -amenities )

# drop amenities that were not used
data <- data[ -c( 43:238 )]

# convert property_type to factor
data <- data %>%
  mutate(f_property_type = factor(property_type))

# convert neighbourhood_cleansed to factor
data <- data %>% 
  mutate( f_neighbourhood = factor(neighbourhood_cleansed))

# add new numeric columns from certain columns
numericals <- c("host_response_rate", "host_acceptance_rate", "host_total_listings_count", "accommodates", "bedrooms", "beds", "number_of_reviews", "review_scores_rating", "calculated_host_listings_count", "reviews_per_month", "bathrooms")                                 

data <- data %>%
  mutate_at(vars(numericals), funs("n"=as.numeric))


# rename columns so they start with n_ as opposed to end with _n
nnames <- data %>%
  select(ends_with("_n")) %>%
  names()
nnames_i <- match(nnames, colnames(data))
colnames(data)[nnames_i] <- paste0("n_", numericals)

# create days since first review
data <- data %>% mutate(
  n_days_since_rv = as.numeric(as.Date(last_scraped,format="%Y-%m-%d") -
                                 as.Date(first_review ,format="%Y-%m-%d")))

# create dummies ----------------------------------------------------------

# create dummies
data <- data %>% mutate(
  d_superhost = ifelse( host_is_superhost == 't', 1, 0),
  d_profile_pic = ifelse( host_has_profile_pic == 't', 1, 0),
  d_identity_verified = ifelse( host_identity_verified == 't', 1, 0),
  d_instant_bookable = ifelse( instant_bookable == 't', 1, 0)
)

# rename amenities dummies
dummies <- c( "wifi", "tv", "refrigerator", "air_conditioning", "baby", "stove","microwave", "free_parking", "paid_parking" )
data <- data %>%
  mutate_at(vars(dummies), funs("d"= (.)))
# rename columns
dnames <- data %>%
  select(ends_with("_d")) %>%
  names()
dnames_i <- match(dnames, colnames(data))
colnames(data)[dnames_i] <- paste0("d_", tolower(gsub("[^[:alnum:]_]", "",dummies)))

# filter needed variables -------------------------------------------------

# keep columns if they start with d_, n_, f_ and some others
data <- data %>%
  select(matches("^d_.*|^n_.*|^f_.*|^p_.*|^usd_.*"), price, id,
         neighbourhood_cleansed, property_type)

#save the data frame
write_csv(data,"~/Desktop/BA-Courses/Data_Analysis3/Assignment2/data/clean/shanghai_clean.csv")


# check if price is missing
nrow(data %>% filter( is.na(price)))

###### Price ######

summary(data$price)
describe(data$price)

# Take log of price
data <- data %>%
  mutate(ln_price = log(price))

data <- data %>%
  filter(price <1300)

# Price Distribution
price_hist <- ggplot(data, aes( x = price)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)),fill = "cyan", color = "black") +
  theme_bw() +
  scale_y_continuous(labels = label_percent()) +
  ylab("Percent") + 
  xlab("Price")

ln_price_hist <- ggplot(data, aes( x = ln_price)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)),fill = "cyan", color = "black") +
  theme_bw() +
  scale_y_continuous(labels = label_percent()) +
  ylab("Percent") + 
  xlab("Price (log)")

price_hist_grid <- ggarrange(
  price_hist,
  ln_price_hist,
  nrow = 1
)

#### feature engineering
## check distrubutions and functional forms for OLS and OLS with LASSO
# check categorical variables ---------------------------------------------

categoricals <- c("f_property_type", "f_neighbourhood")

for (i in 1:length(categoricals)) {
  data %>%
    group_by(get(categoricals[i])) %>%
    summarise(mean_price = mean(price) ,  n=n()) %>%
    print
}

# check dummy variables ---------------------------------------------------

dummies <- c( "d_wifi", "d_tv", "d_refrigerator", "d_air_conditioning", 
              "d_microwave", "d_baby", "d_stove", "d_free_parking", "d_paid_parking",
              "d_superhost", "d_profile_pic", "d_identity_verified", "d_instant_bookable")

for (i in 1:length(dummies)) {
  data %>%
    group_by(get(dummies[i])) %>%
    summarise(mean_price = mean(price) ,  n=n()) %>%
    print
}

# check numerical variables -----------------------------------------------

### n_accommodates
data %>%
  group_by(n_accommodates) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

ggplot(data = data, aes(x=n_accommodates, y=price)) +
  geom_point(size=1, shape=16)+
  geom_smooth(method="lm",  se=FALSE)+
  labs(x="Number of people accommodated",y="Price")

# the chart shows a linear relationship so leave as is

### n_beds
data %>%
  group_by(n_beds) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

# impute missing and 0 with n_accommodates assume they mean the same
data <- data %>% 
  mutate(n_beds = ifelse(is.na(n_beds), n_accommodates, n_beds),
         n_beds = ifelse( n_beds == 0, n_accommodates, n_beds))
# check the relationship with price
ggplot( data, aes( x = n_beds, y = price ) ) +
  geom_smooth( method = "loess", se=F, color = brewer.pal( 3, "Set2" )[1], size = 1.5 ) +
  theme_bw()
# add square of n_beds
data <- data %>% mutate( n_beds2 = n_beds^2)

# regression 1: price and number of reviews
reg1<-lm(price ~ n_beds, data=data)
summary(reg1)
# regression 2: log-price and log number of reviews
reg2<-lm(price ~ n_beds + n_beds2, data=data)
summary(reg2)

# add square n_beds to the model

### n_bathrooms
data %>%
  group_by(n_bathrooms) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

ggplot(data, aes(n_bathrooms)) +
  geom_histogram( alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("N of bathrooms")

# impute missing with median
data <- data %>%
  mutate( n_bathrooms =  ifelse(is.na(n_bathrooms), median(n_bathrooms, na.rm = T), n_bathrooms))

# check the relationship with price
ggplot( data, aes( x = n_bathrooms, y = price ) ) +
  geom_smooth( method = "loess", se=F, color = brewer.pal( 3, "Set2" )[1], size = 1.5 ) +
  theme_bw()

# add log bathrooms to df
data <- data %>% mutate( ln_bathrooms = log(n_bathrooms))

# use log bathrooms in the model

### n_number of reviews
describe(data$n_number_of_reviews)

# filter the df to check the distribution of number of reviews
nreview_plot <- data %>%
  filter(n_number_of_reviews < 100)

ggplot(nreview_plot, aes(n_number_of_reviews)) +
  geom_histogram(binwidth = 5, alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("Number of reviews")

# use logs as well because the distribution is very skewed
data <- data %>%
  mutate(ln_number_of_reviews = log(n_number_of_reviews+1))

ggplot(data, aes(ln_number_of_reviews)) +
  geom_histogram(binwidth = 0.5, alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("Log Number of reviews")

# regression 3: price and number of reviews
reg3<-lm(price ~ n_number_of_reviews, data=data)
summary(reg3)
# regression 4: price and log number of reviews
reg4<-lm(price ~ ln_number_of_reviews, data=data)
summary(reg4)

# use log number of reviews in the model

### n_days_since_rv
describe(data$n_days_since_rv)

# check the relationship with price
ggplot( data, aes( x = n_days_since_rv, y = price ) ) +
  geom_smooth( method = "loess", se=F, color = brewer.pal( 3, "Set2" )[1], size = 1.5 ) +
  theme_bw()

# add square to df
data <- data %>%
  mutate(
    n_days_since_rv2=n_days_since_rv^2)

# use square in the model


### n_review_scores_rating
describe(data$n_review_scores_rating)

# check the relationship with price
ggplot( data, aes( x = n_review_scores_rating, y = price ) ) +
  geom_smooth( method = "loess", se=F, color = brewer.pal( 3, "Set2" )[1], size = 1.5 ) +
  theme_bw()

# leave as is

### n_host_acceptance_rate
describe(data$n_host_acceptance_rate)

# check the relationship with price
ggplot( data, aes( x = n_host_acceptance_rate, y = price ) ) +
  geom_smooth( method = "loess", se=F, color = brewer.pal( 3, "Set2" )[1], size = 1.5 ) +
  theme_bw()

# add square to df
data <- data %>%
  mutate(
    n_host_acceptance_rate2=n_host_acceptance_rate^2)

### n_host_response_rate
describe(data$n_host_response_rate)

# check the relationship with price
ggplot( data, aes( x = n_host_response_rate, y = price ) ) +
  geom_smooth( method = "loess", se=F, color = brewer.pal( 3, "Set2" )[1], size = 1.5 ) +
  theme_bw()

# add square to df
data <- data %>%
  mutate(
    n_host_response_rate2=n_host_response_rate^2)

### n_host_total_listings_count
describe(data$n_host_total_listings_count)

# check the relationship with price
ggplot( data, aes( x = log(n_host_total_listings_count), y = price ) ) +
  geom_smooth( method = "loess", se=F, color = brewer.pal( 3, "Set2" )[1], size = 1.5 ) +
  theme_bw()

# add log to df
data <- data %>%
  mutate(
    ln_host_total_listings_count=log(n_host_total_listings_count))

### n_bedrooms
describe(data$n_bedrooms)

# check the relationship with price
ggplot( data, aes( x = n_bedrooms, y = price ) ) +
  geom_smooth( method = "loess", se=F, color = brewer.pal( 3, "Set2" )[1], size = 1.5 ) +
  theme_bw()

# add square to df
data <- data %>%
  mutate(
    n_bedrooms2=n_bedrooms^2)

# use square as well

### n_reviews_per_month
describe(data$n_reviews_per_month)

# check the relationship with price
ggplot( data, aes( x = n_reviews_per_month, y = price ) ) +
  geom_smooth( method = "loess", se=F, color = brewer.pal( 3, "Set2" )[1], size = 1.5 ) +
  theme_bw()

# leave as is

# change infinite values to NaNs
for (j in 1:ncol(data) ) data.table::set(data, which(is.infinite(data[[j]])), j, NA)

# check missing values ----------------------------------------------------

to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

# impute without flags where there are only a few missing
data <- data %>% 
  mutate( n_host_total_listings_count = ifelse(is.na(n_host_total_listings_count), median(n_host_total_listings_count, na.rm = T), n_host_total_listings_count),
          ln_bathrooms = ifelse(is.na(ln_bathrooms), 1, ln_bathrooms),
          n_bedrooms = ifelse(is.na(n_bedrooms),n_beds%/%2, n_bedrooms)) # assume that there are two beds in a bedroom 

# redo their polinomials and logs
data <- data %>% 
  mutate(ln_host_total_listings_count = log(n_host_total_listings_count+1),
         n_bedrooms2 = n_bedrooms^2)

# impute with flags where there are more missing
data <- data %>%
  mutate(
    flag_days_since_rv=ifelse(is.na(n_days_since_rv),1, 0),
    n_days_since_rv =  ifelse(is.na(n_days_since_rv), median(n_days_since_rv, na.rm = T), n_days_since_rv),
    flag_review_scores_rating=ifelse(is.na(n_review_scores_rating),1, 0),
    n_review_scores_rating =  ifelse(is.na(n_review_scores_rating), median(n_review_scores_rating, na.rm = T), n_review_scores_rating),
    flag_reviews_per_month=ifelse(is.na(n_reviews_per_month),1, 0),
    n_reviews_per_month =  ifelse(is.na(n_reviews_per_month), median(n_reviews_per_month, na.rm = T), n_reviews_per_month),
    flag_number_of_reviews=ifelse(n_number_of_reviews==0,1, 0),
    flag_host_response_rate = ifelse(is.na(n_host_response_rate), 1, 0),
    n_host_response_rate = ifelse(is.na(n_host_response_rate), median(n_host_response_rate, na.rm = T), n_host_response_rate),
    flag_host_acceptance_rate = ifelse(is.na(n_host_acceptance_rate), 1, 0),
    n_host_acceptance_rate = ifelse(is.na(n_host_acceptance_rate), median(n_host_acceptance_rate, na.rm = T), n_host_acceptance_rate)
  )

# redo their polinomials and logs
data <- data %>% mutate(
  n_days_since_rv2 = n_days_since_rv^2,
  n_host_acceptance_rate2 = n_host_acceptance_rate^2,
  n_host_response_rate2 = n_host_response_rate^2
)
