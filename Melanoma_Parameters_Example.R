library(tidyverse) #please load dependence
library(readxl) #please load dependence
library(tlbparam) #R package for thermogram parameters

# import thermograms using tlbparam package for proper important structure
melTherms <- load_thermograms(file = "data-raw/Mela_Publication_Data.xlsx", # file path and name
                              sheet='DSC Data',
                              temps = c(paste0('T', seq(45, 90, by=0.1))))

# run clean_thermograms for calculation of all thermogram parameters
thermSummaryAll <- clean_thermograms(melTherms)

# extract the thermogram parameters from produced data.frame
thermogram.parameters <- thermSummaryAll %>% select('SampleCode', 'Width':'TPeak F')
str(thermogram.parameters)
