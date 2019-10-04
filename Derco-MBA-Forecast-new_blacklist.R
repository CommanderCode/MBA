library(plyr)
library(prophet)
library(lubridate)
library(lazyeval)
library(tidyverse)
library(data.table)

########################################
########## Creating Forecast ###########
########################################

## Bring in the data
datasource = data.frame(fread('Datasources/Suzuki_all.csv'))
blacklist = data.frame(fread('DataSources/Parts-Blacklist.csv'))


## Remove noise materials (such as services)
datasource <- datasource %>%
  filter(!datasource$Material.Text %in%
           blacklist$Part &
           Invoice != '#')


## Explicitly cast date parts and truncate to month format (mm/01/yyyy)
datasource$Year.Month.natural.Text = datasource$Year.Month.natural.Text %>%
  as.Date(format = "%m/%d/%Y") %>%
  floor_date('month')


## Create list of recent materials - must have Invoices between 2016 and 2018
## This removes all material that have not been sold since 2015 (removes noise from model)
listOfRecent = datasource %>%
  filter(year(Year.Month.natural.Text) >= 2016 & year(Year.Month.natural.Text) <= 2018) %>%
  group_by(Material.Text, Year.Month.natural.Text) %>%
  count()


## Aggregates materials by month/year
listOfRecent = listOfRecent %>%
  group_by(Material.Text) %>%
  count()


## Keep only materials that have at least 24 observations in the last 30 months
## This removes all materials that have inconsistent usages (removes poor data)
listOfRecent = listOfRecent %>%
  filter(n >= 30)


# Aggregate by material text, get counts sold in that month
datasource <- datasource %>%
  filter(Material.Text %in% listOfRecent$Material.Text) %>%
  group_by(Year.Month.natural.Text, Material.Text) %>%
  count() 

datasource = data.frame("ds" = datasource$Year.Month.natural.Text,
                        "y" = datasource$n, 
                        "part" = datasource$Material.Text) 


## Create forecast function to iterate each Material through
buildForecast = function (datasource, product) {
  ## Set current date to July 2018 because of the dataset
  current <- as.Date('2018/07/01')
  
  
  ## Select relevant columns from datasource
  df = subset(datasource, part == product, select = c("ds", "y"))
  df$cap = 999999
  df$floor = 0
  
  
  ## Build and execute predictions
  m = prophet(df, weekly.seasonality=FALSE, daily.seasonality=FALSE)
  future = make_future_dataframe(m, periods = 12, freq = 'month')
  future$cap = 999999
  future$floor = 0
  forecast = predict(m, future)
  
  
  ## Differentiate observed values from predicted ones
  values_observed <- df %>%
    filter(ds<current & year(ds)>=2012) %>%
    arrange(ds) %>% select(ds,y) %>%
    mutate(isForecast = 0)
  
  values_forecast <- forecast %>%
    filter(ds>=current) %>%
    arrange(ds) %>%
    select(ds,yhat) %>%
    mutate(isForecast = 1)
  
  
  ## Manipulate the data to fit our time series model 
  colnames(values_forecast) <- c('ds', 'y', 'isForecast')
  values <- rbind(values_observed, values_forecast) %>%
    mutate(model = product)
  values <- values[, c(4,1,2,3)]
  values$y <- round(values$y)
  values
}


## For loop iterating through material texts, forecasting using the buildforecast function
output <- data.frame()
for (x in listOfRecent$Material.Text) {
  output <- rbind(output,buildForecast(datasource, x))
}

## Write results to .csv for visualization
write.csv(output, file = 'Datasources/Derco_Forecast_Materials.csv')

