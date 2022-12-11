library(readxl)
library(tidyverse)
library(tlb.param)

# import patient data using tlbparam package
patientData <- load_patientdata(
  "temp/Clinical and demographic data 11.03.2022corrected.xlsx",
  sheet = "Sheet 1")

patientData$`Sample ID` = as.numeric(patientData$`Sample ID`)
typeof(patientData$`Sample ID`)

# import thermograms using tlbparam package
melTherms <- load_thermograms("temp/Melanoma thermograms All RB.xlsx")
melTherms$SampleCode = as.numeric(melTherms$SampleCode)

allData <- inner_join(
                      patientData,
                      melTherms,
                      by = c("Sample ID" = "SampleCode")
                      )
thermSummaryAll <- clean_thermograms(melTherms)

patientTest <- clean_thermograms(melTherms, patient = c(4501, 5402))
