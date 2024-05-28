rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(purrr)
library(haven)
library(tidyverse)
library(foreign)
library(data.table)
library(readxl)


in_files <- list.files(path="/Users/nominbayars/Диплом/data/", pattern = c("18", ".dta"))
in_files






df2 <- map_df(in_files[1], read_dta) 
df9 <- map_df(in_files[7], read_dta)
df12 <- map_df(in_files[8], read_dta)
df13 <- map_df(in_files[9], read_dta)
df15 <- map_df(in_files[10], read_dta)
df16 <- map_df(in_files[11], read_dta)

df7 <- map_df(in_files[6], read_dta)
df6_2 <- map_df(in_files[4], read_dta)
df6_3 <- map_df(in_files[5], read_dta)
df6_1 <- map_df(in_files[2], read_dta)
nf_index <- read_excel('nonfoodindex.xlsx', sheet = 3)

df6_1[is.na(df6_1)] = 0
df6_2[is.na(df6_2)] = 0
df6_3[is.na(df6_3)] = 0

# - - - - - - - - - - - - - - Household income - - - - - - - - - - - - - - - - #
ls_inc_1 <- df6_1 %>% group_by(identif) %>% summarise(total = sum(q0605))
ls_inc_2 <- df6_2 %>% group_by(identif) %>% summarise(total = sum(q0612, q0614))
crop_inc <- df6_3 %>% group_by(identif) %>% summarise(total = sum(q0623b))
priv_inc <- df7 %>% group_by(identif) %>% summarise(inc = sum(q0710))

salary <- df2 %>% select(identif, q0417b, q0417c, q0419b, q0419c)
salary[is.na(salary)] = 0
salary <- salary %>% group_by(identif) %>% 
  summarise(total = sum(q0417b, q0417c, q0419b, q0419c))
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

# - - - - - - - - - - - - - - Food expenditure - - - - - - - - - - - - - - - - #
food_c <- df16 %>% group_by(identif, item) %>% 
  summarise(total = sum(q1201_1, q1202_1, q1203_1)*12,
            price = sum(q1204))
food_p <- food_c[which(food_c$price != 0),] %>% group_by(item) %>% summarise(price = mean(price))

food_c <- food_c[-4]
food_c <- left_join(food_c, food_p, by = 'item')

food_exp <- food_c %>% group_by(identif, item) %>% summarise(tot_exp = total * price)
food_exp[is.na(food_exp)] = 0
food_1 <- food_exp %>% group_by(identif) %>%  filter(item %in% 10101:11204 & tot_exp != 0) %>% summarise(tot_exp = sum(tot_exp))
food_2 <- food_exp %>% group_by(identif) %>%  filter(item %in% 11301:11405 & tot_exp != 0) %>% summarise(tot_exp = sum(tot_exp))
food_c_exp <- data.frame(identif = unique(food_exp$identif))
food_c_exp <- left_join(food_c_exp, food_1, by = 'identif')
food_c_exp <- left_join(food_c_exp, food_2, by = 'identif')
food_c_exp[is.na(food_c_exp)] = 0
colnames(food_c_exp) <- c('identif', 'food', 'alch-cig')
# - - - - - - - - - - - - - - Food expenditure - - - - - - - - - - - - - - - - #

# - - - - - - - - - - - - Non-food expenditure - - - - - - - - - - - - - - - - #
nf_list <- list(c(20101:20810), c(20901:21419), c(21501:21509),
                c(21601:21714), c(21801:21813), c(21901:22301),
                c(22401:22406), c(22501:22505), c(22601:24309))

df15[is.na(df15)] = 0
nonfood_exp <- data.frame(identif = unique(df15$identif))

for (i in nf_list){
  info = df15 %>% group_by(identif) %>% filter(item %in% i)
  info = info %>% group_by(identif) %>% 
    summarise(total = sum(q1103, q1104, q1105))
  nonfood_exp <- left_join(nonfood_exp, info, by = 'identif')
}
colnames(nonfood_exp) <- c('identif', 'clothing', 'home', 'med', 'transport',
                           'comm', 'holiday', 'educ', 'hotel', 'other')
# - - - - - - - - - - - - Non-food expenditure - - - - - - - - - - - - - - - - #

# - - - - - - - - - - - Energy and housing expenditure - - - - - - - - - - - - #
df12[is.na(df12)] = 0
df13[is.na(df13)] = 0
energy <- df12 %>% group_by(identif) %>% 
  summarise(total = sum(q0940, q0941, q0942))
housing <- df13 %>% group_by(identif) %>% 
  summarise(total = sum(q0945, q0946, q0947))
energy <- left_join(energy, housing, by = 'identif')
en_hs <- energy %>% group_by(identif) %>% summarise(total = sum(total.x, total.y))
# - - - - - - - - - - - Energy and housing expenditure - - - - - - - - - - - - #
nonfood_exp <- left_join(nonfood_exp, en_hs, by = 'identif')
colnames(nonfood_exp)[11] <- 'energy-housing'

total_exp <- left_join(nonfood_exp, food_c_exp, by = 'identif')
total_exp <- na.omit(total_exp)


total_exp <- total_exp %>% group_by(identif) %>% 
  mutate(total = sum(c_across(c(1:12))))

total_exp <- left_join(total_exp, income, by = 'identif')
colnames(total_exp)[14:15] <- c('total_exp', 'income')

summary(total_exp$total_exp)
sd(total_exp$total_exp)
total_exp[is.na(total_exp)] = 0

summary(na.omit(total_exp$income))
sd(total_exp$income)
inc <-  income[which(income$identif %in% total_exp$identif),]
plot(total_exp$income, total_exp$total_exp)
sd(total_exp$income)
total_exp$income
summary(income$total)

summary(inc$total)
sd(inc$total)
food_max <- food_c %>% filter(total != 0) %>% group_by(item) %>% summarise(mean(total))
summary(inc$total)
a <- table(bread$q1204) %>% data.frame()
mean(bread$q1204) * 1.1
mean(bread$q1204) * 0.9
bread <- df16 %>% filter(item == 10202, q1204 != 0)
bread <- bread[c(1, length(bread))]
sd(bread$q1204)
mean(bread$q1204)
WriteXLS::WriteXLS(bread, 'bread.xlsx')

plot(bread$identif, bread$q1204)
sd(bread$q1204)

341 / 1759

bread %>% filter(q1204 >= 7500)

food_c %>% filter(item == 10101)
sd(income$total)
stats::sd(total_exp$total_exp)
total_exp$income
plot(total_exp$identif, total_exp$total_exp)
food_item <- food_exp %>% group_by(item) %>% summarise(tot_exp = mean(tot_exp))
flour <- food_item %>% filter(item %in% 10101:10115)
meat <- food_item %>% filter(item %in% 10201:10217)
sea <- food_item %>% filter(item %in% 10301:10304)
dairy <- food_item %>% filter(item %in% 10401:10415)
oil <- food_item %>% filter(item %in% 10501:10508)
fruit <- food_item %>% filter(item %in% 10601:10609)
veg <- food_item %>% filter(item %in% 10701:10714)
bean <- food_item %>% filter(item %in% 10801:10806)
sugar <- food_item %>% filter(item %in% 10901:10913)
other <- food_item %>% filter(item %in% 11001:11008)
tea <- food_item %>% filter(item %in% 11101:11106)
drinks <- food_item %>% filter(item %in% 11201:11204)
alch <- food_item %>% filter(item %in% 11301:11306)
cig <- food_item %>% filter(item %in% 11401:11405)
sum(cig$tot_exp) / sum(food_item$tot_exp) 
0.76 + 1.9


total_exp <- total_exp[order(total_exp$income),]
total_exp
group1 <- total_exp[1:1794,]
group2 <- total_exp[1795:3588,]
group3 <- total_exp[3589:5382,]
group4 <- total_exp[5383:7176,]
group5 <- total_exp[7177:8969,]

gw1 <- food_exp[which(food_exp$identif %in% group1$identif),]
gw2 <- food_exp[which(food_exp$identif %in% group2$identif),]
gw3 <- food_exp[which(food_exp$identif %in% group3$identif),]
gw4 <- food_exp[which(food_exp$identif %in% group4$identif),]
gw5 <- food_exp[which(food_exp$identif %in% group5$identif),]
w1 <- gw1 %>% group_by(item) %>% summarise(mean = mean(tot_exp))
w1['mean'] <- w1['mean'] / mean(group1$total_exp)
w2 <- gw2 %>% group_by(item) %>% summarise(mean = mean(tot_exp))
w2['mean'] <- w2['mean'] / mean(group2$total_exp)
w3 <- gw3 %>% group_by(item) %>% summarise(mean = mean(tot_exp))
w3['mean'] <- w3['mean'] / mean(group3$total_exp)
w4 <- gw4 %>% group_by(item) %>% summarise(mean = mean(tot_exp))
w4['mean'] <- w4['mean'] / mean(group4$total_exp)
w5 <- gw5 %>% group_by(item) %>% summarise(mean = mean(tot_exp))
w5['mean'] <- w5['mean'] / mean(group5$total_exp)

w1 <- left_join(w1, food_p, by = 'item')
w2 <- left_join(w2, food_p, by = 'item')
w3 <- left_join(w3, food_p, by = 'item')
w4 <- left_join(w4, food_p, by = 'item')
w5 <- left_join(w5, food_p, by = 'item')
w1[is.na(w1)] = 0
w2[is.na(w2)] = 0
w3[is.na(w3)] = 0
w4[is.na(w4)] = 0
w5[is.na(w5)] = 0
w1['index'] <- w1['mean'] * w1['price']
w2['index'] <- w2['mean'] * w2['price']
w3['index'] <- w3['mean'] * w3['price']
w4['index'] <- w4['mean'] * w4['price']
w5['index'] <- w5['mean'] * w5['price']
w_mean <- food_exp %>% group_by(item) %>% summarise(mean = mean(tot_exp))

g1 <- left_join(g1, nf_index, by = 'Item')
g5 <- left_join(g5, nf_index, by = 'Item')
g2 <- left_join(g5, nf_index, by = 'Item')
g3 <- left_join(g5, nf_index, by = 'Item')
g4 <- left_join(g5, nf_index, by = 'Item')
g5[is.na(g5)] = 0
g1[is.na(g1)] = 0
g2[is.na(g2)] = 0
g3[is.na(g3)] = 0
g4[is.na(g4)] = 0
w_mean <- left_join(w_mean, food_p, by = 'item')
w_mean['mean'] <- w_mean['mean'] / mean(total_exp$total_exp)
w_mean[is.na(w_mean)] = 0
w_mean['index'] <- w_mean['mean'] * w_mean['price']

price_data <- read_excel('price_data.xlsx')
colnames(w1)[2] <- 'q1'
colnames(w2)[2] <- 'q2'
colnames(w3)[2] <- 'q3'
colnames(w4)[2] <- 'q4'
colnames(w5)[2] <- 'q5'
colnames(w_mean) <- 'q_mean'

maindata <- left_join(price_data, w1[1:2], by = 'item')
maindata <- left_join(maindata, w2[1:2], by = 'item')
maindata <- left_join(maindata, w3[1:2], by = 'item')
maindata <- left_join(maindata, w4[1:2], by = 'item')
maindata <- left_join(maindata, w5[1:2], by = 'item')

maindata <- left_join(maindata, w_mean[1:2], by = 'item')

WriteXLS::WriteXLS(maindata, 'mean_weight.xlsx')

sum(g1['Expenditure']  / 100 * g1[3]) + sum(w1$index)
sum(g4['Expenditure']  / 100 * g4[3]) + sum(w4$index)
sum(g3['Expenditure']  / 100 * g3[3]) + sum(w3$index)
sum(g5['Expenditure']  / 100 * g5[3]) + sum(w5$index)
sum(g2['Expenditure']  / 100 * g2[3]) + sum(w2$index)
sum(mean_g['Expenditure']  / 100 * mean_g['2018']) + sum(w_mean$index)

mean_g <- left_join(mean_g, nf_index, by = 'Item')
mean_g[is.na(mean_g)] = 0

sum(w2$index)
sum(w3$index)
sum(w4$index)
sum(w5$index)
plot(inc$identif, inc$total)
inc %>% filter(total < 1000000)
inc %>% filter(total >= 1000000, total < 5000000)
inc %>% filter(total >= 5000000, total < 10000000)
inc %>% filter(total >= 10000000, total < 50000000)

table(inc$total) %>% view()

total_exp[which(total_exp$identif %in% group1$identif),]
# Quantile 
g1 <- data.frame(matrix(nrow = 12, ncol = 2))
colnames(g1) <- c('Item', 'Expenditure')
g1[1] <- c('clothing', 'home', 'med', 'transport', 'comm', 'holiday',
           'educ', 'hotel', 'other', 'energy-housing', 'food', 'alch-cig')
g1[2] <- c(mean(group1$clothing), mean(group1$home), mean(group1$med), 
           mean(group1$transport),
           mean(group1$comm), mean(group1$holiday), mean(group1$educ), 
           mean(group1$hotel),
           mean(group1$other), mean(group1$`energy-housing`), mean(group1$food), 
           mean(group1$`alch-cig`))
g1[2] <- g1[2] / mean(group1$total_exp) * 100

g2 <- data.frame(matrix(nrow = 12, ncol = 2))
colnames(g2) <- c('Item', 'Expenditure')

g2[1] <- c('clothing', 'home', 'med', 'transport', 'comm', 'holiday',
           'educ', 'hotel', 'other', 'energy-housing', 'food', 'alch-cig')
g2[2] <- c(mean(group2$clothing), mean(group2$home), mean(group2$med), 
           mean(group2$transport),
           mean(group2$comm), mean(group2$holiday), mean(group2$educ), 
           mean(group2$hotel),
           mean(group2$other), mean(group2$`energy-housing`), mean(group2$food), 
           mean(group2$`alch-cig`))
g2[2] <- g2[2] / mean(group2$total_exp) * 100

g3 <- data.frame(matrix(nrow = 12, ncol = 2))
colnames(g3) <- c('Item', 'Expenditure')

g3[1] <- c('clothing', 'home', 'med', 'transport', 'comm', 'holiday',
           'educ', 'hotel', 'other', 'energy-housing', 'food', 'alch-cig')
g3[2] <- c(mean(group3$clothing), mean(group3$home), mean(group3$med), 
           mean(group3$transport),
           mean(group3$comm), mean(group3$holiday), mean(group3$educ), 
           mean(group3$hotel),
           mean(group3$other), mean(group3$`energy-housing`), mean(group3$food), 
           mean(group3$`alch-cig`))
g3[2] <- g3[2] / mean(group3$total_exp) * 100
g3

g4 <- data.frame(matrix(nrow = 12, ncol = 2))
colnames(g4) <- c('Item', 'Expenditure')

g4[1] <- c('clothing', 'home', 'med', 'transport', 'comm', 'holiday',
           'educ', 'hotel', 'other', 'energy-housing', 'food', 'alch-cig')
g4[2] <- c(mean(group4$clothing), mean(group4$home), mean(group4$med), 
           mean(group4$transport),
           mean(group4$comm), mean(group4$holiday), mean(group4$educ), 
           mean(group4$hotel),
           mean(group4$other), mean(group4$`energy-housing`), mean(group4$food), 
           mean(group4$`alch-cig`))
g4[2] <- g4[2] / mean(group4$total_exp) * 100
g4

g5 <- data.frame(matrix(nrow = 12, ncol = 2))
colnames(g5) <- c('Item', 'Expenditure')

g5[1] <- c('clothing', 'home', 'med', 'transport', 'comm', 'holiday',
           'educ', 'hotel', 'other', 'energy-housing', 'food', 'alch-cig')
g5[2] <- c(mean(group5$clothing), mean(group5$home), mean(group5$med), 
           mean(group5$transport),
           mean(group5$comm), mean(group5$holiday), mean(group5$educ), 
           mean(group5$hotel),
           mean(group5$other), mean(group5$`energy-housing`), mean(group5$food), 
           mean(group5$`alch-cig`))
g5[2] <- g5[2] / mean(group5$total_exp) * 100
g5

mean_g <- data.frame(matrix(nrow = 12, ncol = 2))
colnames(mean_g) <- c('Item', 'Expenditure')

mean_g[1] <- c('clothing', 'home', 'med', 'transport', 'comm', 'holiday',
           'educ', 'hotel', 'other', 'energy-housing', 'food', 'alch-cig')
mean_g[2] <- c(mean(total_exp$clothing), mean(total_exp$home), mean(total_exp$med), 
           mean(total_exp$transport),
           mean(total_exp$comm), mean(total_exp$holiday), mean(total_exp$educ), 
           mean(total_exp$hotel),
           mean(total_exp$other), mean(total_exp$`energy-housing`), mean(total_exp$food), 
           mean(total_exp$`alch-cig`))
mean_g[2] <- mean_g[2] / mean(total_exp$total_exp) * 100
mean_g



