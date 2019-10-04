library(plyr)
library(prophet)
library(lubridate)
library(lazyeval)
library(tidyverse)
library(data.table)
library(arules)

########################################
########## Creating Forecast ###########
########################################

#######################
### CREATE THE DATA ###
#######################
# ALL SUZUKI FILESCOMBINED
original_datasource <- data.frame(fread('Datasets/Suzuki_all.csv'))
original_datasource$Year.Month.natural.Text = original_datasource$Year.Month.natural.Text %>%
  as.Date(format = "%m/%d/%Y") %>%
  floor_date('month')

## Remove noise materials (such as services)
blacklist1 <- data.frame(fread('Datasets/Parts-Blacklist.csv'))
blacklist2 <- data.frame(fread('Datasets/Blacklist-For-Jorge.csv',sep = '|',quote=""))
blacklist <- c(blacklist1$Part,blacklist2$Material,'KIT MOTOSIERRA 66,8 CC ESPADA 20""""') %>% unique()

##filter data by blacklist, min.date, max.date, freq (number of times a material is sold in the last 2.5 years)
create_data <- function(original_datasource, blacklist, min.date, freq){
  index <- which(original_datasource$Material.Text %in% blacklist)
  index <- c(index, which(original_datasource$Invoice=='#'))
  datasource <- original_datasource[-index,]
  ## paste Material Text and ID
  datasource <- datasource %>% mutate(Material.Text= paste(datasource$Material.Text,' (',datasource$Material,')',sep = ''))
  
  ## Keep only baskets that have at least x(freq) observations in the last 30 months
  ## This removes all materials that have inconsistent usages (removes poor data)
  min.date <- min.date %>% as.Date(format = "%m/%d/%Y")
  listOfRecent = datasource %>%
    filter(Year.Month.natural.Text >= min.date)
  listOfRecent = listOfRecent %>% group_by(Year.Month.natural.Text, Material.Text) %>% tally()
  
  
  ## Create list of recent materials - must have Invoices between 2016 and 2018
  ## This removes all material that have not been sold since 2015 (removes noise from model)
  month.counts <- listOfRecent$Material.Text %>% table() %>% as.data.frame() %>% filter(Freq>=freq)
  
  ## keep all parts that were sold in x(freq) of last 30 months
  datasource<- datasource %>%
    filter(Material.Text %in% month.counts$.)
  
  ## gather baskets (long run time, maybe 2 minutes)
  datasource <- ddply(datasource,c("Invoice","Year.Month.natural.Text"),
                      function(df1)paste(df1$Material.Text,
                                         collapse = "|"))
  ## rename column from V1 to Material.Text
  colnames(datasource)[3] <- "Material.Text"
  
  listOfRecent = datasource %>% select(Year.Month.natural.Text, Material.Text) %>%
    filter(Year.Month.natural.Text >= min.date)
  listOfRecent = listOfRecent %>% group_by(Year.Month.natural.Text, Material.Text) %>% tally()
  
  ## Create list of recent materials - must have Invoices between 2016 and 2018
  ## This removes all material that have not been sold since 2015 (removes noise from model)
  month.counts <- listOfRecent$Material.Text %>% table() %>% as.data.frame() %>% filter(Freq>=freq)
  
  ## keep all parts that were sold in x(freq) of last 30 months
  datasource<- datasource %>%
    filter(Material.Text %in% month.counts$.)
  
  ## Aggregates materials by month/year and material.Text, get counts sold in that month
  datasource = datasource %>% group_by(Year.Month.natural.Text, Material.Text) %>% tally()

  ## return dataframe of 3 columns, and rows for every month for each material:  
  ## ds=date by months, y = sales by month, part = material.text
  data.frame("ds" = datasource$Year.Month.natural.Text,
             "y" = datasource$n, 
             "part" = datasource$Material.Text)
  }

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

## run the create_data function
## parameters:  original_datasource, blacklist, min.date ('m/1/yyyy'), freq (number of times a material is sold in the last 2.5 years)
min.date <- '1/1/2016'
freq <- 18
datasource<- create_data(original_datasource,blacklist,min.date, freq)

## For loop iterating through material texts, forecasting using the buildforecast function
output <- data.frame()
for (x in unique(datasource$part)) {
  output <- rbind(output,buildForecast(datasource, x))
}

filename <- paste(c('Datasets/Derco_Forecast_Baskets.freq_', freq,'TextAndID.csv'),collapse = '')
output$y[which(output$y<0)] <- 0
## Write results to .csv for visualization
fwrite(output, file = filename)
