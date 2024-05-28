rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(purrr)
library(haven)
library(tidyverse)
library(foreign)
library(data.table)
library(readxl)
library(WriteXLS)
in_files <- list.files(path="/Users/nominbayars/Диплом/data/", pattern = c("19", ".dta"))



id_inc <- read_excel('id.xlsx')
df2 <- map_df(in_files[1], read_dta) 
df9 <- map_df(in_files[7], read_dta)
df12 <- map_df(in_files[8], read_dta)
df13 <- map_df(in_files[9], read_dta)
df15 <- map_df(in_files[10], read_dta)
# df16 <- map_df(in_files[11], read_dta)
df7 <- map_df(in_files[6], read_dta)
df6_2 <- map_df(in_files[4], read_dta)
df6_3 <- map_df(in_files[5], read_dta)
df6_1 <- map_df(in_files[2], read_dta)
nf_index <- read_excel('nonfoodindex.xlsx', sheet = 3)
df6_1[is.na(df6_1)] = 0
df6_2[is.na(df6_2)] = 0
df6_3[is.na(df6_3)] = 0

# - - - - - - - - - - - - - - Household income - - - - - - - - - - - - - - - - #

ls_inc_1 <- df6_1 %>% group_by(identif) %>% summarise(total = sum(q0305))
ls_inc_2 <- df6_2 %>% group_by(identif) %>% summarise(total = sum(q0312, q0314))
crop_inc <- df6_3 %>% group_by(identif) %>% summarise(total = sum(q0323b))

priv_inc <- df7 %>% group_by(identif) %>% summarise(inc = sum(q0410))
view(df2)
salary <- df2 %>% select(identif, q0210b, q0210c, q0212b, q0212c)
salary[is.na(salary)] = 0
salary <- salary %>% group_by(identif) %>% 
  summarise(total = sum(q0210b, q0210c, q0212b, q0212c))
view(df9)
income <- df9 %>% 
  select(identif, q0504:q0511, q0513:q0521, q0523:q0526, q0530:q0535, q0537:q0542)
income[is.na(income)] = 0
income <- income %>% group_by(identif) %>% summarise(total = sum(c_across(c(1:33))))
income <- left_join(salary, income, by = 'identif')
income <- left_join(income, ls_inc_1, by = 'identif')
income <- left_join(income, ls_inc_2, by = 'identif')
income <- left_join(income, crop_inc, by = 'identif')
income <- left_join(income, priv_inc, by = 'identif')
income[is.na(income)] = 0
income <- income %>% group_by(identif) %>% summarise(total = sum(c_across(c(1:6))))
income <- income %>% filter(total != 0)
income <- income[order(income$total),]

# - - - - - - - - - - - - - - Household income - - - - - - - - - - - - - - - - #
food_p <- food %>% group_by(identif, item) %>% 
  summarise(price = sum(q0904))
food_p <- food_p[which(id_inc$identif %in% food_p$identif),]

id <- unique(food_p$identif)
food_p <- food_p %>% filter(price != 0)
income <- income[which(income$identif %in% id),]

7149:(7148+1787)


p1 <- food_p %>% filter(identif %in% id_inc$identif[1:1787]) %>% group_by(item) %>% 
  summarise(p1 = mean(price))
p2 <- food_p %>% filter(identif %in% income$identif[1788:3574]) %>% group_by(item) %>% 
  summarise(p2 = mean(price))
p3 <- food_p %>% filter(identif %in% income$identif[3575:5361]) %>% group_by(item) %>% 
  summarise(p3 = mean(price))
p4 <- food_p %>% filter(identif %in% income$identif[5362:7148]) %>% group_by(item) %>% 
  summarise(p4 = mean(price))
p5 <- food_p %>% filter(identif %in% income$identif[7149:nrow(income)]) %>% group_by(item) %>% 
  summarise(p5 = mean(price))

price <- data.frame(matrix(nrow = 129))
price['item'] <- unique(food_p$item)
price <- left_join(price, p5, by = 'item')
price <- price[-1]

WriteXLS(income, 'id.xlsx')
