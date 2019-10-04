

## read in transactions
txns <- read.transactions(file = "./tmp/orders.csv", 
                          format = "single", sep = ",", cols=c(2,1), skip=1, rm.duplicates = FALSE)
txns.list <- as(txns,'list')
## convert transactions to transaction ID lists
txns_list <- as(txns, "tidLists")
f <- eclat(txns, parameter = list(support = 0.001, tidLists = FALSE))
st <- supportingTransactions(f,txns) %>%as('list')

## change to data.frame
txns_df <- data.frame(transaction.IDs=seq(1,length(st),1), basket=names(st),Invoice=I(st))


# NOT RUN {
## Create transaction data set.
library(arules)
library(tidyverse)
data <- list(
  c("a","b","c"),
  c("a","b","c"),
  c("a","b"),
  c("a","b","d"),
  c("b","e"),
  c("b","c","e"),
  c("a","d","e"),
  c("a","c"),
  c("a","b","d"),
  c("c","e"),
  c("a","b","d","e")
)
data <- as(data, "transactions")
inspect(data)

## convert transactions to transaction ID lists
tl <- as(data, "tidLists")
inspect(tl)

## mine itemsets with transaction ID lists
f <- eclat(data, parameter = list(support = 0, tidLists = TRUE))
tl2 <- tidLists(f)
inspect(tl2)

f <- eclat(data, parameter = list(support = 0), tidList=FALSE)
inspect(f)

## find supporting Transactions
st <- supportingTransactions(f, data)
inspect(st)
as(st, "list")

# }