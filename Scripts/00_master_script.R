#########################################################
#   Master script: HPIs sampling weights paper
#########################################################

p_list<-c("data.table", "chilemapas", "tidyverse","censo2017","foreign")

#---- Part 1: Data Cleaning & Descriptive statistics
# Note: This scripts cleans the TocToc.com data. This data is not available for public use due to contract restrictions.

source("Scripts/01_data_cleanning_toctoc.R")#TocToc housing data (not publicly available given proprietary restrictions)
source("Scripts/01_1_DescriptiveStatisticsTocToc.R")

#---- Part 2: Data Assembling

source("Scripts/02_1_Assembling Census data - Frequency-based sampling weights.R") # run once

viviendas_vacantes_rm<-fread(file='01 Data/viviendas_vacantes_rm.csv')

#---- Part 3: Sampling weights estimation

source("Scripts/03_1_Sampling weights - Frequency based.R")

#---- Part 4: HPI estimation

#---- Part 5: Results gathering and visualization
source("Scripts/01_1_Figures.R")

