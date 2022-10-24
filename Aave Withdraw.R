#remove(list=ls())

# First, let's load a few useful packages
library(tidyverse)
library(httr)
library(jsonlite)
library(scales)
library(waffle) 
library(ggrepel)


# Parameters

EtherScanAPIToken <- "R6AF2VP9RSGQTMKWAZ3D7QR3JVAJCHFWZK"

Address <- "0x7d2768de32b0b80b7a3454c06bdac94a69ddc7a9" #lending pool contract for aave v2

Topic0 <- "0x3115d1449a7b732c986cba18244e897a450f61e1bb8d589cd2e69e6c8924f9f7" #withdraw event

# Retrieve the last 10000 transactions (maximum allowed by Etherscan) from the OpenSea contract

a <- data.frame()
block <- 0

now <- Sys.time()
for (i in 1:100000000){
resEtherScan_logs <- GET("https://api.etherscan.io/api",
                           query = list(module="logs", 
                                        action="getLogs",
                                        fromBlock= block,
                                        toBlock = "",
                                        topic0= Topic0,
                                        address= Address,
                                        apikey=EtherScanAPIToken))
  
  
  dataEtherScan_logs <- fromJSON(rawToChar(resEtherScan_logs$content), flatten=TRUE)$result
  
  if (is.null(dim(dataEtherScan_logs))) {
    break
  }
  
  b <- dataEtherScan_logs %>% 
    mutate(reserve = paste0("0x",substring(unlist(dataEtherScan_logs$topics)[seq(2,dim(dataEtherScan_logs)[1]*4,4)],27,67)),
           user = paste0("0x",substring(unlist(dataEtherScan_logs$topics)[seq(3,dim(dataEtherScan_logs)[1]*4,4)],27,67)),
           to = paste0("0x",substring(unlist(dataEtherScan_logs$topics)[seq(3,dim(dataEtherScan_logs)[1]*4,4)],27,67)),
           amount = as.numeric(data),
           contract_address = Address,
           evt_tx_hash = transactionHash,
           evt_block_time = as.Date(as.POSIXct(as.numeric(timeStamp), origin="1970-01-01")),
           evt_block_number = as.numeric(blockNumber)) %>%
    select(reserve,user,to,amount,contract_address,evt_tx_hash,evt_block_time,evt_block_number)
  
  
  
  a <- rbind(a,b) %>% distinct()
  
  block <- max(b$evt_block_number)
  
}

time_passed <- Sys.time() - now
time_passed

saveRDS(a,"C:\\Users\\Alex\\Desktop\\Crypto Jobs\\Aave analysis\\Input\\aave_v2_withdraw.RData")


