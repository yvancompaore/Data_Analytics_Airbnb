library(shiny)
library(stringr)
library(shinydashboard)
library(ggplot2) 
library(dplyr)

prepare_data <- function(city,linstings_url,calendar_url,data_date,row)
{
  print("preparind data for :")
  print(paste("city:",city))
  print(paste("linstings_url",linstings_url))
  print(paste("calendar_url",calendar_url))
  
  
  #read url
  listings <- read.csv(textConnection(readLines(gzcon(url(linstings_url$listings_url)))))
  print("toto")
  calendar <- read.csv(textConnection(readLines(gzcon(url(calendar_url$calendar_url)))))
  print("tata")
  
  
  ## Add Keys: columns city and day date
  listings$city <- city$city
  print("tataaaaa")
  listings$data_date <- data_date$data_date
  
  print("tata2")
  
  ## Select interesting columns
  ### Most columns don't contain interesting information
  columns_listings <- c("city", "data_date", "id", "neighbourhood_cleansed", 
                        "latitude", "longitude", 
                        "property_type", "room_type", "accommodates", "bedrooms", 
                        "beds", "price", "minimum_nights",  "maximum_nights")
  print("tata3")
  
  listings <- listings %>% 
    select(columns_listings) %>% 
    arrange(id)
  
  print("titi")
  # Cleaning calendar dataframe
  
  ## arrange by id and date
  calendar <- calendar %>% 
    arrange(listing_id, date)
  
  ## add day number (starting first day)
  calendar <- calendar %>%
    group_by(listing_id) %>%
    mutate(day_nb = row_number()) %>%
    ungroup()
  
  ## change available column to binary
  calendar <- calendar %>%
    mutate(available = ifelse(available=="t", 1, 0))
  
  ## clean price column and transform to numeric
  calendar <- calendar %>%
    mutate(price = str_replace(price, "\\$", ""),
           adjusted_price = str_replace(adjusted_price, "\\$", ""))
  calendar <- calendar %>%
    mutate(price = str_replace(price, ",", ""),
           adjusted_price = str_replace(adjusted_price, ",", ""))
  calendar <- calendar %>%
    mutate(price = as.numeric(price),
           adjusted_price = as.numeric(adjusted_price))
  
  ## calculate estimated revenue for upcoming day
  calendar <- calendar %>%
    mutate(revenue = price*(1-available))
  
  ## calculate availability, price, revenue for next 30, 60 days ... for each listing_id
  calendar <- calendar %>%
    group_by(listing_id) %>%
    summarise(availability_30 = sum(available[day_nb<=30], na.rm = TRUE),
              #availability_60 = sum(available[day_nb<=60], na.rm = TRUE),
              #availability_90 = sum(available[day_nb<=90], na.rm = TRUE),
              #availability_365 = sum(available[day_nb<=365], na.rm = TRUE),
              price_30 = mean(price[day_nb<=30 & available==0], na.rm = TRUE),
              #price_60 = mean(price[day_nb<=60 & available==0], na.rm = TRUE),
              #price_90 = mean(price[day_nb<=90 & available==0], na.rm = TRUE),
              #price_365 = mean(price[day_nb<=365 & available==0], na.rm = TRUE),
              revenue_30 = sum(revenue[day_nb<=30], na.rm = TRUE),
              #revenue_60 = sum(revenue[day_nb<=60], na.rm = TRUE),
              #revenue_90 = sum(revenue[day_nb<=90], na.rm = TRUE),
              #revenue_365 = sum(revenue[day_nb<=365], na.rm = TRUE)           
    )
  
  print("okkk")
  
  listings_cleansed <- listings %>% left_join(calendar, by = c("id" = "listing_id"))
  
  dir.create(file.path("data_cleansed", city, data_date), recursive = TRUE)
  
  write.csv(listings_cleansed, file.path("data_cleansed", city, data_date, "listings.csv"))
  print(paste0("saving data into ", file.path("data_cleansed", city, data_date, "listings.csv")))
  
}  

all_data <- read.csv("all_data.csv", stringsAsFactors = FALSE)
country_interest <- c("germany")
all_data <- all_data %>% filter(country %in% country_interest)
all_data <- slice_head( all_data %>%  group_by(city),n=3)
print(all_data$city)

for(row in 1:nrow(all_data)){
  prepare_data(all_data[row,"city"],all_data[row,"listings_url"],all_data[row,"calendar_url"],all_data[row,"data_date"],row)
}
