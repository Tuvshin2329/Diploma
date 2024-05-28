rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(purrr)
library(haven)
library(tidyverse)
library(foreign)
library(data.table)
in_files <- list.files(path="/Users/nominbayars/Диплом/data/", pattern = c("urb_diary", ".dta"))
code <- read_excel('Calculation.xlsx', sheet = 2)
code <- code[1:2]
colnames(code) <- c('item', 'id')
code <- na.omit(code)


df18 <- map_df(in_files[1], read_dta) 
df19 <- map_df(in_files[2], read_csv) 
df20_1 <- map_df(in_files[5], read_excel)  
df20_2 <- map_df(in_files[7], read_excel) 
df20 <- rbind(df20_1, df20_2)
df20 <- df20[-1]
df21 <- map_df(in_files[9], read_dta)  
df22 <- map_df(in_files[10], read_dta)  
df20$q1303 <- as.integer(df20$q1303)
df18[is.na(df18)] = 0
df19[is.na(df19)] = 0
df20[is.na(df20)] = 0
df21[is.na(df21)] = 0
df22[is.na(df22)] = 0
df19 <- df19[-1]

#----------------------------------- 2018 -------------------------------------#
price18 <- df18[which(df18$q1204 != 0) ,c(1,2, length(df18))]
price18 <- price18 %>% group_by(item) %>% summarise(price = mean(q1204))
#----------------------------------- 2019 -------------------------------------#
price19 <- df19[which(df19$q0904 != 0) ,c(1,2, length(df19))]
price19 <- price19 %>% group_by(item) %>% summarise(price = mean(q0904))
#----------------------------------- 2020 -------------------------------------#
price20 <- df20[which(df20$q1303 != 0) ,c(1,2, 5)]
price20 <- price20 %>% group_by(item) %>% summarise(price = mean(q1303))
price20 <- left_join(price20, code, by = 'item')
price20 <- price20 %>% relocate(id)
price20 <- price20[-2]
colnames(price20)[1] <- 'item'

#----------------------------------- 2021 -------------------------------------#
price21 <- df21[which(df21$q1303 != 0) ,c(1,2, 5)]
price21 <- price21 %>% group_by(item) %>% summarise(price = mean(q1303))
#----------------------------------- 2022 -------------------------------------#
price22 <- df22[which(df22$q1303 != 0) ,c(1,2, 5)]
price22 <- price22 %>% group_by(item) %>% summarise(price = mean(q1303))

price_data <- data.frame(matrix(nrow = 132, ncol = 1))
colnames(price_data) <- c('item')
price_data[1] <- code$id
price_data <- left_join(price_data, price18, by = 'item')

price_data <- left_join(price_data, price19, by = 'item')

price_data <- left_join(price_data, price20, by = 'item')
price_data
price_data <- left_join(price_data, price21, by = 'item')
price_data <- left_join(price_data, price22, by = 'item')
view(price_data)
WriteXLS::WriteXLS(price_data, 'price_data.xlsx')
