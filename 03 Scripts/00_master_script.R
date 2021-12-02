#########################################################
#   Master script: HPIs sampling weights paper
#   Author: Esteban Lopez Ochoa, PhD.
#   Inst: The University of Texas at San Antonio
#########################################################

p_list<-c("data.table", "chilemapas", "tidyverse","censo2017","foreign")

#---- Part 1: Data Cleaning
# Note: This scripts cleans the TocToc.com data. This data is not available for public use due to contract restrictions.

source("01_DataCleaning.R")

#---- Part 2: Data Assembling

source("03 Scripts/02_1_Assembling Census data - Frequency-based sampling weights.R")

#---- Part 3: Sampling weights estimation

source("03 Scripts/03_1_Sampling weights - Frequency based.R")

#---- Part 4: HPI estimation

#---- Part 5: Results gathering and visualization

