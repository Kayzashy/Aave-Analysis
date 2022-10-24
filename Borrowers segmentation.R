#remove(list=ls())

# First, let's load a few useful packages
library(tidyverse)
library(httr)
library(jsonlite)
library(scales)
library(waffle) 
library(ggrepel)

### How to get actual time with hours/minutes/seconds: a little example

# library(lubridate)
# seconds_passed <- as.numeric(head(dataEtherScan_logs$timeStamp))
# my_date <- as.POSIXct("1970-01-01 00:00:00")   
# my_date + seconds(seconds_passed)  

# Input

deposits <- readRDS("C:\\Users\\Alex\\Desktop\\Crypto Jobs\\Aave analysis\\Input\\aave_v2_deposits.RData")

withdraw <- readRDS("C:\\Users\\Alex\\Desktop\\Crypto Jobs\\Aave analysis\\Input\\aave_v2_withdraw.RData")

borrows <- readRDS("C:\\Users\\Alex\\Desktop\\Crypto Jobs\\Aave analysis\\Input\\aave_v2_borrows.RData")

repay <- readRDS("C:\\Users\\Alex\\Desktop\\Crypto Jobs\\Aave analysis\\Input\\aave_v2_repay.RData")

liquidations <- readRDS("C:\\Users\\Alex\\Desktop\\Crypto Jobs\\Aave analysis\\Input\\aave_v2_liquidations.RData")

prices <- readRDS("C:\\Users\\Alex\\Desktop\\Crypto Jobs\\Aave analysis\\Input\\aave_v2_prices.RData") %>% rename("evt_block_time" = timestamp)

all_evt <- list(deposits,borrows,withdraw,repay,liquidations)


deposits %>% filter(user == "0xa53fe221bd861f75907d8fd496db1c70779721aa")
borrows %>% filter(user == "0xa53fe221bd861f75907d8fd496db1c70779721aa")
liquidations %>% filter(user == "0xa53fe221bd861f75907d8fd496db1c70779721aa")

remove_clients <- c()

for (i in 1:length(all_evt)){
a <- all_evt[[i]] %>% 
     left_join(prices, by = c("evt_block_time","reserve")) %>%
     filter(is.na(symbol)) %>% select(user) %>% distinct()
remove_clients = rbind(remove_clients,a) %>% distinct()    
}

remove_clients

deposits_na <- deposits %>% 
  left_join(prices, by = c("evt_block_time","reserve")) %>%
  filter(is.na(symbol)) %>% select(user) %>% distinct()

borrows_na <- borrows %>% 
  left_join(prices, by = c("evt_block_time","reserve")) %>%
  filter(is.na(symbol)) %>% select(user) %>% distinct()

repay_na <- repay %>% 
  left_join(prices, by = c("evt_block_time","reserve")) %>%
  filter(is.na(symbol)) %>% select(user) %>% distinct()

deposits %>% filter(!(user %in% deposits_na))

prices %>% group_by(reserve) %>% summarise(min_data = min(evt_block_time)) %>% arrange(min_data) %>% top_n(1)

# Filtering only for 10 top borrowed and lended assets

deposits_top <- deposits %>% 
  left_join(prices, by = c("evt_block_time","reserve")) %>% 
  mutate(amount_usd = (amount * price)/10^decimals)

deposits_top %>% filter(is.na(decimals)) %>% select("evt_block_time","reserve")

prices %>% filter(reserve == "0x8798249c2e607446efb7ad49ec89dd1865ff4272")




borrows_top <- borrows %>% 
  left_join(prices, by = c("evt_block_time","reserve")) %>% 
  filter(!is.na(symbol)) %>% 
  mutate(amount_usd = (amount * price)/10^decimals)

prices %>% filter(symbol== "WETH")

min(prices$evt_block_time)
# Some stats

total_deposits <- sum(deposits_top$amount_usd)
total_borrows <- sum(borrows_top$amount_usd)


deposits_top %>% ggplot(aes(x=evt_block_time,y=amount_usd)) + geom_line()



