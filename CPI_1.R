rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(purrr)
library(haven)
library(tidyverse)
library(foreign)
library(data.table)
library(readxl)
files <- list.files(path="/Users/nominbayars/Диплом/data/", pattern = c("18",".dta"))

id <- read_excel('Calculation.xlsx', sheet = 2)
nf_id <- id[3:4]
f_id <- id[1:2] %>% na.omit()
colnames(f_id) <- c('item', 'code')
# food_1 <- map_df(files[16], read_excel)
# food_2 <- map_df(files[18], read_excel)
# food <- rbind(food_1, food_2)
food <- map_df(files[11], read_dta)

# food <- left_join(food, f_id, by = 'item')
# food <- food[-c(1,3)]

# colnames(food)[7] <- 'item'
# food$q1303 <- as.double(food$q1303)
# food$q1302 <- as.double(food$q1302)
food[is.na(food)] = 0

# energy <- map_df(files[3], read_csv)
# housing <- map_df(files[4], read_csv)
# nonfood_1 <- map_df(files[9], read_csv)
# nonfood_2 <- map_df(files[10], read_csv)
# nonfood_3 <- map_df(files[11], read_csv)
# nonfood_4 <- map_df(files[12], read_csv)
# nonfood_5 <- map_df(files[13], read_csv)
# nf_id['item'] <- unique(nonfood_1$item)
# nonfood <- rbind(nonfood_1, nonfood_2)
# nonfood <- rbind(nonfood, nonfood_3)
# nonfood <- rbind(nonfood, nonfood_4)
# nonfood <- rbind(nonfood, nonfood_5)
# nonfood <- left_join(nonfood, nf_id[-1], by = 'item')
# nonfood <- nonfood[-c(1,3:5)]
# colnames(nonfood)[4] <- 'item'
energy <- map_df(files[8],read_dta)
housing <- map_df(files[9], read_dta)
nonfood <- map_df(files[10], read_dta)
nonfood[is.na(nonfood)] = 0


nonfood['q0804'] <- nonfood['q1104'] + nonfood['q1105']
nf_df <- nonfood %>% group_by(item) %>% summarise(paid = mean(q1103),
                                         free = mean(q0804))
energy
energy <- energy[c(1:2, 8:10)]

energy[is.na(energy)] = 0
energy['q0723'] <- energy['q0941'] + energy['q0942']
energy <- energy %>% group_by(energy_id) %>% summarise(paid = mean(q0940),
                                        free = mean(q0723))


housing <- housing[c(1:2, 5:7)]

housing[is.na(housing)] = 0
# energy['q0728'] <- energy['q0728'] + energy['q0729']
housing['q0728'] <- housing['q0946'] + housing['q0947']
housing <- housing %>% group_by(item) %>% summarise(paid = mean(q0945),
                                                       free = mean(q0728))
energy <- energy %>% summarise(paid = sum(paid),
                               free = sum(free))
housing <- housing %>% summarise(paid = sum(paid),
                               free = sum(free))
nonfood_pf <- nf_df %>% summarise(paid = sum(paid), 
                                  free = sum(free))

food['q1302'] <- food['q1201_2'] + food['q1202_2'] + food['q1203_2']
food['q1304'] <- food['q1201_3'] + food['q1202_3'] + food['q1203_3']
food['q1305'] <- food['q1201_4'] + food['q1202_4'] + food['q1203_4']
food['q1303'] <- food['q1204']
f_df <- food %>% group_by(item) %>% summarise(paid = mean(q1302),
                                      free = mean(q1304),
                                      own = mean(q1305))

f_p <- food %>% group_by(item) %>%  filter(q1303 != 0) %>% summarise(price = mean(q1303))
f_df <- left_join(f_df, f_p, by = 'item')
f_df[is.na(f_df)] = 0
f_df <- f_df %>% group_by(item) %>% summarise(paid = paid * price * 12,
                                              free = free * price * 12, 
                                              own = own * price * 12)

food_pfo <- f_df %>% summarise(paid = sum(paid),
                               free = sum(free),
                               own = sum(own))

sum(food_pfo) / sum(food_pfo, nonfood_pf, energy, housing)
huns <- f_df %>% filter(item %in% 10101:11204) %>% summarise(total = sum(paid, free, own))
alch_cig <- f_df %>% filter(item %in% 11301:11405) %>% summarise(total = sum(paid, free, own))
alch_cig / sum(food_pfo, nonfood_pf, energy, housing)
weight <- data.frame()
nf_list <- list(c(20101:20810), c(20901:21419), c(21501:21509),
                c(21601:21714), c(21801:21813), c(21901:22301),
                c(22401:22406), c(22501:22505), c(22601:24309))

for (i in nf_list){
  info = nf_df %>% filter(item %in% i) %>% summarise(total = sum(paid, free))
  weight = rbind(weight, info)

}
weight[nrow(weight)+1,] = sum(energy, housing)
 
weight / sum(food_pfo, nonfood_pf, energy, housing) * 100
food_pfo$paid / sum(food_pfo, nonfood_pf, energy, housing) * 100
food_pfo$free / sum(food_pfo, nonfood_pf, energy, housing) * 100
food_pfo$own / sum(food_pfo, nonfood_pf, energy, housing) * 100
sum(nonfood_pf$free, energy$free, housing$free) / sum(food_pfo, nonfood_pf, energy, housing) * 100
sum(nonfood_pf$paid, energy$paid, housing$paid) / sum(food_pfo, nonfood_pf, energy, housing) * 100
f_list <- list(c(10101:10115), c(10201:10217), c(10301:10304), c(10401:10415),
               c(10501:10508), c(10601:10609), c(10701:10715), c(10801:10805),
               c(10901:10913), c(11001:11010), c(11101:11105), c(11201:11204),
               c(11301:11306), c(11401:11405))

f_df['total'] <- f_df['paid'] + f_df['free'] + f_df['own']
weight_food <- data.frame()

for (i in f_list){
  info = f_df %>% filter(item %in% i) %>% summarise(total = sum(total))
  weight_food = rbind(weight_food, info)
  
}
weight_food / sum(weight_food) * 100
f_df / sum(food_pfo, nonfood_pf, energy, housing) * 100


