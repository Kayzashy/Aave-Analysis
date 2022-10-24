#remove(list=ls())

# First, let's load a few useful packages
library(tidyverse)
library(httr)
library(jsonlite)
library(scales)
library(waffle) 
library(ggrepel)
library(crypto2)

# Input

deposits <- readRDS("C:\\Users\\Alex\\Desktop\\Crypto Jobs\\Aave analysis\\Input\\aave_v2_deposits.RData")

borrows <- readRDS("C:\\Users\\Alex\\Desktop\\Crypto Jobs\\Aave analysis\\Input\\aave_v2_borrows.RData")

# Getting the address of borrowed and deposited assets

key = "d56dc15a-efb1-4744-8b37-c3d1edfe3719"
coinmarketcapr::setup(api_key = key)

addresses_all <- coinmarketcapr::get_crypto_map() %>% filter(!is.na(platform_token_address)) %>% 
  arrange(rank) %>% 
  select(symbol, name,platform_token_address) %>%
  rename("reserve" = platform_token_address) %>% mutate(reserve = tolower(reserve)) %>%
  mutate(reserve = case_when(symbol == "BUSD" ~ "0x4fabb145d64652a948d72533023f6e7a623c7c53",
                            TRUE ~ reserve))


addresses_aave <- rbind(deposits,borrows) %>% 
  select(reserve) %>% 
  distinct() %>% 
  left_join(addresses_all, by = "reserve") 

### Get prices of those assets

symbols <- addresses_aave$symbol

min_date <- min(deposits$evt_block_time) - months(1)

symbols_list <- crypto_list() %>% filter(symbol %in% symbols)

symbols_prices_raw <- crypto_history(
  coin_list = symbols_list,
  convert = "USD",
  limit = NULL,
  start_date = min_date,
  end_date = NULL,
  interval = "daily",
  sleep = 2,
  finalWait = TRUE
)

symbols_prices_raw <- symbols_prices_raw %>% filter(name %in% addresses_aave$name)

### Decimals 

token_info <- read_excel("C:\\Users\\Alex\\Desktop\\Crypto Jobs\\Aave analysis\\Input\\Tokens info.xlsx", sheet = 1) %>% select(symbol,name,decimals) 

#token_info %>% filter(name %in% addresses_aave$name)

### Join together

symbols_prices <- symbols_prices_raw %>%
  mutate(timestamp = as.Date(timestamp)) %>%
  select(timestamp,name,symbol,open) %>%
  rename("price" = open) %>% 
  left_join(addresses_aave, by = c("symbol")) %>% 
  left_join(token_info, by = "symbol")


saveRDS(symbols_prices,"C:\\Users\\Alex\\Desktop\\Crypto Jobs\\Aave analysis\\Input\\aave_v2_prices.RData")
