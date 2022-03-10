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

data <- data %>% separate(neighbourhood_cleansed , " / " ,
                into = c("garbage","neighbourhood_cleansed") )

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

#save workfile
write_csv(data,"~/Desktop/BA-Courses/Data_Analysis3/Assignment2/data/clean/shanghai_workfile.csv")

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

# airbnb prediction -----------------------------------------------------------------------------------

# descriptive statistics --------------------------------------------------

# boxplot of price by property type
ggplot(data = data, aes(x = f_property_type, y = price)) +
  stat_boxplot(aes(group = f_property_type), geom = "errorbar", width = 0.3, size = 0.5, na.rm=T)+
  geom_boxplot(aes(group = f_property_type),
               size = 0.5, width = 0.6, alpha = 0.3, na.rm=T, outlier.shape = NA)+
labs(x = "Property type",y = "Price")

price_diff_by_variables2 <- function(data, factor_var, dummy_var, factor_lab, dummy_lab){
  # Looking for interactions.
  # It is a function it takes 3 arguments: 1) Your dataframe,
  # 2) the factor variable (like room_type)
  # 3)the dummy variable you are interested in (like TV)
  
  # Process your data frame and make a new dataframe which contains the stats
  factor_var <- as.name(factor_var)
  dummy_var <- as.name(dummy_var)
  
  stats <- data %>%
    group_by(!!factor_var, !!dummy_var) %>%
    dplyr::summarize(Mean = mean(price, na.rm=TRUE),
                     se = sd(price)/sqrt(n()))
  
  stats[,2] <- lapply(stats[,2], factor)
  
  ggplot(stats, aes_string(colnames(stats)[1], colnames(stats)[3], fill = colnames(stats)[2]))+
    geom_bar(stat='identity', position = position_dodge(width=0.9), alpha=0.8)+
    geom_errorbar(aes(ymin=Mean-(1.96*se),ymax=Mean+(1.96*se)),
                  position=position_dodge(width = 0.9), width = 0.25)+
    scale_color_manual(name=dummy_lab,
                       values=c('red','blue')) +
    scale_fill_manual(name=dummy_lab,
                      values=c('red','blue')) +
    ylab('Mean Price')+
    xlab(factor_lab) +
    theme_bw()+
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_blank(),
          axis.line=element_line(),
          legend.position = "top",
          #legend.position = c(0.7, 0.9),
          legend.box = "vertical",
          legend.text = element_text(size = 5),
          legend.title = element_text(size = 5, face = "bold"),
          legend.key.size = unit(x = 0.4, units = "cm")
    )
}
# check for interactions --------------------------------------------------

# check property type interactions
p1 <- price_diff_by_variables2(data, "f_property_type", "d_superhost", "Property Type", "Superhost")
p2 <- price_diff_by_variables2(data, "f_property_type", "d_profile_pic", "Property Type", "Profile Picture") # use this
p3 <- price_diff_by_variables2(data, "f_property_type", "d_identity_verified", "Property Type", "Identity Verified")
p4 <- price_diff_by_variables2(data, "f_property_type", "d_instant_bookable" , "Property Type", "Instant Bookable") # use this
p5 <- price_diff_by_variables2(data, "f_property_type", "d_wifi" , "Property Type", "Wifi") # use this
p6 <- price_diff_by_variables2(data, "f_property_type", "d_tv" , "Property Type", "Tv") # use this
p7 <- price_diff_by_variables2(data, "f_property_type", "d_refrigerator", "Property Type", "Refrigerator") # use this
p8 <- price_diff_by_variables2(data, "f_property_type", "d_air_conditioning" , "Property Type", "Air Conditioning")
p9 <- price_diff_by_variables2(data, "f_property_type", "d_microwave", "Property Type", "Microwave")
p10 <- price_diff_by_variables2(data, "f_property_type", "d_baby", "Property Type", "Baby Friendly") # use this
p11 <- price_diff_by_variables2(data, "f_property_type", "d_stove", "Property Type", "Stove") # use this
p12 <- price_diff_by_variables2(data, "f_property_type", "d_free_parking", "Property Type", "Free Parking")
p13 <- price_diff_by_variables2(data, "f_property_type", "d_paid_parking", "Property Type", "Paid Parking") #use this


sum_interactions <- plot_grid(p2, p4, p5, p6, p7, p10, p11, p12, p14, nrow=3, ncol=3)
sum_interactions


# preparation for modeling ------------------------------------------------

# group variables
target_var <- 'price'

basic_vars <- c('f_property_type', 'f_neighbourhood', 'n_accommodates', 'n_bedrooms', 'n_beds', 'd_instant_bookable', 'n_bathrooms')

host_vars <- c( 'n_host_response_rate', 'flag_host_response_rate','n_host_acceptance_rate', 'flag_host_acceptance_rate', 'd_superhost', 'd_profile_pic', 'd_identity_verified', 'n_host_total_listings_count')

reviews <- c(  'n_number_of_reviews', 'flag_number_of_reviews', 'n_days_since_rv', 'flag_days_since_rv', 'n_review_scores_rating', 'flag_review_scores_rating','n_reviews_per_month', 'flag_reviews_per_month')

amenities <- c('d_wifi', 'd_tv', 'd_refrigerator', 'd_air_conditioning', 'd_microwave', 'd_baby', 'd_stove', 'd_free_parking', 'd_paid_parking')

transformed_vars <- c( 'n_beds2', 'ln_bathrooms', 'ln_number_of_reviews', 'n_days_since_rv2', 'n_host_acceptance_rate2', 'n_host_response_rate2', 'ln_host_total_listings_count', 'n_bedrooms2')

X_for_ols <- c('f_property_type * d_profile_pic', 'f_property_type * d_instant_bookable', 'f_property_type * d_wifi', 'f_property_type * d_tv', 'f_property_type * d_refrigerator', 'f_property_type * d_baby', 'f_property_type * d_microwave', 'f_property_type * d_stove', 'f_property_type * d_paid_parking')

X_for_lasso  <- c('f_property_type * d_profile_pic', 'f_property_type * d_identity_verified', 'f_property_type * d_superhost', 'f_property_type * d_instant_bookable', paste0("(f_property_type) * (",                                                                                                                                                                              paste(amenities, collapse=" + "),")"))

# group predictors for models

predictors_1 <- c(basic_vars)
predictors_2 <- c(basic_vars, host_vars, reviews, amenities)
predictors_3 <- c(basic_vars, host_vars, reviews, amenities, transformed_vars)
predictors_4 <- c(predictors_3, X_for_ols)
predictors_5 <- c(predictors_3, X_for_lasso)

# create holdout set
set.seed(890)

train_indices <- as.integer(createDataPartition(data$price, p = 0.8, list = FALSE))
df_train <- data[train_indices, ]
df_holdout <- data[-train_indices, ]

# set the number of folds for cross-validation
train_control <- trainControl(method = "cv",
                              number = 5,
                              verboseIter = FALSE)

# OLS ---------------------------------------------------------------------

# simplest model
set.seed(8)
system.time({
  ols_model1 <- train(
    formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
    data = df_train,
    method = "lm",
    trControl = train_control
  )
})

ols_model_coeffs1 <-  ols_model1$finalModel$coefficients
ols_model_coeffs_df1 <- data.frame(
  "variable" = names(ols_model_coeffs1),
  "ols_coefficient" = ols_model_coeffs1
) %>%
  mutate(variable = gsub("`","",variable))

# model with transformed variables
set.seed(8)
system.time({
  ols_model2 <- train(
    formula(paste0("price ~", paste0(predictors_3, collapse = " + "))),
    data = df_train,
    method = "lm",
    trControl = train_control
  )
})

ols_model_coeffs2 <-  ols_model2$finalModel$coefficients
ols_model_coeffs_df2 <- data.frame(
  "variable" = names(ols_model_coeffs2),
  "ols_coefficient" = ols_model_coeffs2
) %>%
  mutate(variable = gsub("`","",variable))

# model with transformed variables plus interactions
set.seed(8)
system.time({
  ols_model3 <- train(
    formula(paste0("price ~", paste0(predictors_4, collapse = " + "))),
    data = df_train,
    method = "lm",
    trControl = train_control
  )
})

ols_model_coeffs3 <-  ols_model3$finalModel$coefficients
ols_model_coeffs_df3 <- data.frame(
  "variable" = names(ols_model_coeffs3),
  "ols_coefficient" = ols_model_coeffs3
) %>%
  mutate(variable = gsub("`","",variable))

# OLS with LASSO ----------------------------------------------------------

# transformed numeric variables, no interactions
set.seed(8)
system.time({
  lasso_model1 <- train(
    formula(paste0("price ~", paste0(predictors_4, collapse = " + "))),
    data = df_train,
    method = "glmnet",
    preProcess = c("center", "scale"),
    tuneGrid =  expand.grid("alpha" = 1, "lambda" = seq(0.01, 1, by = 0.05)),
    trControl = train_control
  )
})

print(lasso_model1$bestTune$lambda)

lasso_coeffs1 <- coef(
  lasso_model1$finalModel,
  lasso_model1$bestTune$lambda) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  rename(coefficient = `s1`) 

lasso_coeffs_non_null1 <- lasso_coeffs1[!lasso_coeffs1$lasso_coefficient == 0,]

print(nrow(lasso_coeffs_non_null1))

# transformed numeric variables plus interactions
set.seed(8)
system.time({
  lasso_model2 <- train(
    formula(paste0("price ~", paste0(predictors_5, collapse = " + "))),
    data = df_train,
    method = "glmnet",
    preProcess = c("center", "scale"),
    tuneGrid =  expand.grid("alpha" = 1, "lambda" = seq(0.01, 1, by = 0.05)),
    trControl = train_control
  )
})

print(lasso_model2$bestTune$lambda)

lasso_coeffs2 <- coef(
  lasso_model2$finalModel,
  lasso_model2$bestTune$lambda) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  rename(coefficient = `s1`) 

lasso_coeffs_non_null2 <- lasso_coeffs2[!lasso_coeffs1$lasso_coefficient == 0,]

print(nrow(lasso_coeffs_non_null2))

# put the coefficients in one table
regression_coeffs <- merge(ols_model_coeffs_df3, lasso_coeffs_non_null2, by = "variable", all=TRUE)
names(regression_coeffs) <- c('Variable', 'OLS 3', 'LASSO 2')
# compare OLS and LASSO performance

temp_models <-
  list("OLS 1" = ols_model1,
       "OLS 2" = ols_model2,
       "OLS 3" = ols_model3,
       "LASSO 1 (few interactions)" = lasso_model1,
       "LASSO 2 (all interactions)" = lasso_model2)

result_temp <- resamples(temp_models) %>% summary()

# get test RMSE
result_rmse <- imap(temp_models, ~{
  mean(result_temp$values[[paste0(.y,"~RMSE")]])
}) %>% unlist() %>% as.data.frame() %>%
  rename("CV RMSE" = ".")

# get holdout RMSE
result_holdout <- map(temp_models, ~{
  RMSE(predict(.x, newdata = df_holdout), df_holdout[["price"]])
}) %>% unlist() %>% as.data.frame() %>%
  rename("Holdout RMSE" = ".")

# merge the two
result_combined <- cbind(result_rmse, result_holdout )

# calculate number of variables in each model
num_coefs <-  c(
  length(ols_model1$coefnames),
  length(ols_model2$coefnames),
  length(ols_model3$coefnames),
  nrow(lasso_coeffs_non_null1),
  nrow(lasso_coeffs_non_null2))

ncoefs <- as.data.frame(num_coefs, row.names = rownames(result_combined)
) %>% rename("Number of Coefficients" = "num_coefs")

# merge the three
result_combined <- cbind(ncoefs, result_rmse, result_holdout )

# fitted vs actual values for LASSO 2
# target variable
Ylev <- df_holdout[["price"]]

# get predicted values
predictionlev_holdout_pred <- as.data.frame(predict(lasso_model2, newdata = df_holdout))

# rename column
names(predictionlev_holdout_pred) <- "fit"

# Create data frame with the real and predicted values
d <- data.frame(ylev=Ylev, predlev=predictionlev_holdout_pred[,"fit"] )
# Check the differences
d$elev <- d$ylev - d$predlev

# Plot predicted vs price
level_vs_pred <- ggplot(data = d) +
  geom_point(aes(y=ylev, x=predlev),  size = 1,
             shape = 16, alpha = 0.7, show.legend=FALSE, na.rm=TRUE) +
  geom_segment(aes(x = 0, y = 0, xend = 4000, yend =5000), size=0.5, linetype=2) +
  coord_cartesian(xlim = c(0, 4000), ylim = c(0, 5000)) +
  # scale_x_continuous(expand = c(0.01,0.01),limits=c(0, 4000), breaks=seq(0, 350, by=50)) +
  # scale_y_continuous(expand = c(0.01,0.01),limits=c(0, 5000), breaks=seq(0, 350, by=50)) +
  labs(y = "Price ", x = "Predicted price ")
level_vs_pred

# Random Forest -----------------------------------------------------------

# simpler model
tune_grid <- expand.grid(
  .mtry = c(6, 8, 10),
  .splitrule = "variance",
  .min.node.size = c(5, 10)
)

set.seed(8)
system.time({
  rf_model_1 <- train(
    formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
    data = df_train,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity"
  )
})


# more complex model
tune_grid <- expand.grid(
  .mtry = c(8, 10, 12),
  .splitrule = "variance",
  .min.node.size = c(5, 10, 15)
)

set.seed(8)
system.time({
  rf_model_2 <- train(
    formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
    data = df_train,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity"
  )
})
# evaluate random forests -------------------------------------------------
# save results
results <- resamples(
  list(
    model_1  = rf_model_1,
    model_2  = rf_model_2
  ))
# Variable Importance Plots -------------------------------------------------------
rf_tuning_model2 <- rf_model_2$results %>%
  dplyr::select(mtry, min.node.size, RMSE) %>%
  dplyr::rename(nodes = min.node.size) %>%
  spread(key = mtry, value = RMSE)

# tuning parameter choice 1
result_1 <- matrix(c(
  rf_model_1$finalModel$mtry,
  rf_model_2$finalModel$mtry,
  rf_model_1$finalModel$min.node.size,
  rf_model_2$finalModel$min.node.size
),
nrow=2, ncol=2,
dimnames = list(c("Model 1", "Model 2"),
                c("Min vars","Min nodes"))
)

# RMSE of models
result_2 <- matrix(c(mean(results$values$`model_1~RMSE`),
                     mean(results$values$`model_2~RMSE`)
),
nrow=2, ncol=1,
dimnames = list(c("Model 1", "Model 2"),
                c(results$metrics[2]))
)

# grouped variable importance plot
varImp(rf_model_2)$importance
rf_model_var_imp <- varImp(rf_model_2)$importance
imp <-  rf_model_var_imp
rf_model_var_imp_df <- data.frame(varname = names(rf_model_var_imp),imp) %>%
  arrange(desc(imp)) %>%
  mutate(imp_percentage = imp/sum(imp))
rf_model_var_imp_df[rf_model_var_imp_df$varname %in% amenities,]
# grouped variable importance 2
# basic variables, host variables, review variables and amenities are grouped --> TO THE R MARKDOWN
rf_model_var_imp_df <- rf_model_var_imp_df %>% mutate(
  group2 = ifelse(varname %in% amenities, 'amenities',
                  ifelse(varname %in% basic_vars, 'basic_vars',
                         ifelse(varname %in% reviews, 'reviews', 'host_info'))))
rf_model_var_imp_grouped2 <- rf_model_var_imp_df %>%  group_by(group2) %>% summarise(group_imp_sum = sum(imp_percentage)) %>%  arrange(desc(group_imp_sum))
rf_model_var_imp_grouped2_plot <-
  ggplot(rf_model_var_imp_grouped2, aes(x=reorder(group2, group_imp_sum), y=group_imp_sum)) +
  geom_point(color="navyblue", size=1) +
  geom_segment(aes(x=group2,xend=group2,y=0,yend=group_imp_sum), color="navyblue", size=0.7) +
  ylab("Importance (Percent)") +   xlab("Variable Name") +
  coord_flip() +
  # expand=c(0,0),
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=4), axis.text.y = element_text(size=4),
        axis.title.x = element_text(size=4), axis.title.y = element_text(size=4))
rf_model_var_imp_grouped2_plot


# Partial Dependence Plot -------------------------------------------------

pdp_n_acc <- pdp::partial(rf_model_2, pred.var = "n_accommodates", pred.grid = distinct_(df_holdout, "n_accommodates"), train = df_train)
pdp_n_acc_plot <- pdp_n_acc %>%
  autoplot( ) +
  geom_point( size=2) +
  geom_line( size=1) +
  ylab("Predicted price") +
  xlab("Accommodates (persons)") +
  scale_x_continuous(limit=c(2,6), breaks=seq(1,7,1))
pdp_n_acc_plot

# Model selection ---------------------------------------------------------

final_models <-
  list("OLS3" = ols_model3,
       "LASSO (all interactions)" = lasso_model2,
       "Random forest" = rf_model_2)

results <- resamples(final_models) %>% summary()
results

# evaluate final models on holdout set
final_rmse <- imap(final_models, ~{
  mean(results$values[[paste0(.y,"~RMSE")]])
}) %>% unlist() %>% as.data.frame() %>%
  rename("CV RMSE" = ".")

final_holdout <- map(final_models, ~{
  RMSE(predict(.x, newdata = df_holdout), df_holdout[["price"]])
}) %>% unlist() %>% as.data.frame() %>%
  rename("Holdout RMSE" = ".")

final_combined <- cbind(final_rmse, final_holdout)

# Evaluate model on subsets -----------------------------------------------
data_holdout_w_prediction <- df_holdout %>%
  mutate(predicted_price = predict(rf_model_2, newdata = df_holdout))

######### create nice summary table of heterogeneity
a <- data_holdout_w_prediction %>%
  mutate(is_low_size = ifelse(n_accommodates <= 4, "small apt", "large apt")) %>%
  group_by(is_low_size) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = RMSE(predicted_price, price) / mean(price)
  )


b <- data_holdout_w_prediction %>%
  filter(f_neighbourhood %in% c("Pudong", "Huangpu District", "Xuhui District", "Jing'an District", "Songjiang District", "Minhang District")) %>%
  group_by(f_neighbourhood) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = rmse / mean_price
  )

c <- data_holdout_w_prediction %>%
  filter(f_property_type %in% c("rental unit", "serviced apartment","loft")) %>%
  group_by(f_property_type) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = rmse / mean_price
  )


d <- data_holdout_w_prediction %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = RMSE(predicted_price, price) / mean(price)
  )

# Save output
colnames(a) <- c("", "RMSE", "Mean price", "RMSE/price")
colnames(b) <- c("", "RMSE", "Mean price", "RMSE/price")
colnames(c) <- c("", "RMSE", "Mean price", "RMSE/price")
d<- cbind("All", d)
colnames(d) <- c("", "RMSE", "Mean price", "RMSE/price")

line1 <- c("Type", "", "", "")
line2 <- c("Apartment size", "", "", "")
line3 <- c("Neighbourhood", "", "", "")

result_3 <- rbind(line2, a, line1, c, line3, b, d) %>%
  transform(RMSE = as.numeric(RMSE), `Mean price` = as.numeric(`Mean price`),
            `RMSE/price` = as.numeric(`RMSE/price`))

options(knitr.kable.NA = '')
kable(x = result_3, format = "latex", booktabs=TRUE, linesep = "",digits = c(0,2,1,2), col.names = c("","RMSE","Mean price","RMSE/price")) %>%
  cat(.,file= paste0(output, "performance_across_subsamples.tex"))
options(knitr.kable.NA = NULL)
