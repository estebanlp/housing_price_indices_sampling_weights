# Merge expansores casas y departamentos
# Magdalean Cortina
rm(list=ls(all.names = T))

library(data.table)

pathD <-"/Users/magdalena/Dropbox/02 Housing Price Indices/" 

#Monthly
houses <- fread("01 Data/01_Comuna/Houses_controlDB.csv")
apts <- fread("01 Data/01_Comuna/Apts_controlDB.csv")

#Quarterly
houses <- fread("01 Data/01_Comuna/Houses_controlDB_Q.csv")
apts <- fread("01 Data/01_Comuna/Apts_controlDB_Q.csv")

toctoc <- fread("01 Data/base_toctoc.csv")

ha = list(houses,apts)
houses_apts <- rbindlist(ha)

toctoc_exp <- merge(houses_apts, toctoc, by = "id", all = TRUE)
write.csv(toctoc_exp, file = "01 Data/01_Comuna/base_toctoc_exp.csv")
write.csv(toctoc_exp, file = "01 Data/01_Comuna/base_toctoc_exp_Q.csv")

toc <- read_csv("/Users/magdalena/Dropbox/CEPR/02 Housing Price Indices/01 Data/01_Comuna/base_toctoc_exp.csv")
