library(tidyverse)
load('r-image/Clin_and_Thermo_11172022.RData')


### Exatracting Clinical Information for combined use with Thermograms
AllStatus <- ClinDemo %>% 
  select(`Sample ID`, `PatientStudyID`, `Group`, `StatusSimplified`,
         `TreatmentSimplified`, `Baseline`, `Collected(daysPostdx)`,
         `Age`, `Sex`, `Ethnicity`, `PFS`, `PFS_S`, `OS`, `OS_S`) %>% 
  mutate(`Sample ID` = as.character(`Sample ID`)) %>%
  rename(`Time.days` = `Collected(daysPostdx)`)


### Combine Thermorgrams and Clinical Information
MelaThermo.Clin.long <- left_join(MelaThermo.long, AllStatus, 
                             by=c('SampleID' = 'Sample ID'))


###########################
### GENSUM Metrics and PCA
###########################

### Apply GenSum to thermograms
library(GenSumv20)

####  Need to reshape thermogram dataframe for GenSum
#### Can use this to filter samples if we want to reduce for PCA
#### Could also filter anything else that comes up down the road

#### All thermogram summaries use 45 - 90 C

MelaThermo.GenSum_form <- MelaThermo.long %>% 
  mutate(Temperature = paste0('T', Temperature)) %>%
  pivot_wider(names_from = Temperature, values_from = dCp) %>%
  rename(SampleCode = SampleID)

Summary_Metrics <- clean_thermograms(MelaThermo.GenSum_form)

### tail(colnames(Summary_Metrics), 19) #included metrics

Metric_Summary <- Summary_Metrics %>% 
  dplyr::select(SampleCode, tarea:median) 

###$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$###
### PCA using all thermogram 45 - 90C ###
#########################################

MelaThermo.PC_form <- MelaThermo.long %>% 
  filter(between(Temperature, 45, 90)) %>%
  pivot_wider(names_from = Temperature, values_from = dCp) %>%
  select(-SampleID) %>% as.data.frame()

PCA.out <- prcomp(t(MelaThermo.PC_form))
### Graphing PCs and loadings
# plot(PCA.out$x[,1], type='l') 
# for(j in 2:5) lines(PCA.out$x[,j])
# PCA.out$rotation[,1:6]
Mela.PCs <- PCA.out$rotation[,1:6]


### Add PCs to Summary Information

Mela.GenSum <- cbind(Metric_Summary, Mela.PCs)

Mela.GenSum <- full_join(AllStatus, Mela.GenSum, by=c('Sample ID' = 'SampleCode'))



#### Clean up Data Sets loaded
rm(list=setdiff(ls(), c('ClinDemo', 'Mela.GenSum', 'MelaThermo', 'MelaThermo.Clin.long')))
#save.image('r-image/GenSum_Joins_11162022.RData')
#load('r-image/GenSum_Joins.RData')






### Thermogram Progression for a select patient



M054.Samples <- ClinDemo %>% filter(PatientStudyID == 'M098') %>% 
  select(`Sample ID`) %>% pull()

# MelaThermo.long <- MelaThermo %>% pivot_longer(names_to = 'SampleID', values_to = 'dCp', cols = 2:800)
# 
# which(sapply(MelaThermo, class)!='numeric')
# 
# MelaThermo %>% select('10115') %>% pull()
##
# There is one sample found to have a single missing entry from excel file...
# I replaced the missing value with the measurement at 21.1C

MelaThermo.long %>% filter(between(Temperature, 45, 90)) %>% filter(SampleID %in% M054.Samples) %>%
  ggplot(aes(x = Temperature, y = dCp)) + 
  geom_line(aes(color = DiseaseStatus, group = SampleID, linetype= TreatmentStatus)) + 
  geom_vline(xintercept = 63)


MelaThermo.long %>% filter(Temperature == 63) %>% 
  filter(SampleID %in% M054.Samples) %>% 
  mutate(Order = 1:length(SampleID)) %>%
  #mutate(Order = c(12,1489,1509,1556,1580,1600)) %>% 
  ggplot(aes(x = Order, y = dCp)) + 
  geom_point(aes(color = DiseaseStatus, 
                 shape = TreatmentStatus), size=3)





### Visualization of high-count samples


### Patient M118 ###
Thermo.Samples <- ClinDemo %>% filter(PatientStudyID == 'M118') %>% select(`Sample ID`) %>% pull()

MelaThermo.long %>% filter(between(Temperature, 45, 90)) %>% filter(SampleID %in% Thermo.Samples) %>%
  ggplot(aes(x = Temperature, y = dCp)) + geom_line(aes(color = SampleID, group = SampleID)) + 
  geom_vline(xintercept = 63)

MelaThermo.long %>% filter(Temperature == 63) %>% 
  filter(SampleID %in% Thermo.Samples) %>% 
  mutate(Order = 1:length(SampleID)) %>%
  #mutate(Order = c(12,1489,1509,1556,1580,1600)) %>% 
  ggplot(aes(x = Order, y = dCp)) + geom_point()

### Patient M078 ###
Thermo.Samples <- ClinDemo %>% filter(PatientStudyID == 'M078') %>% select(`Sample ID`) %>% pull()

MelaThermo.long %>% filter(between(Temperature, 45, 90)) %>% filter(SampleID %in% Thermo.Samples) %>%
  ggplot(aes(x = Temperature, y = dCp)) + geom_line(aes(color = SampleID, group = SampleID)) + 
  geom_vline(xintercept = 63)

MelaThermo.long %>% filter(Temperature == 63) %>% 
  filter(SampleID %in% Thermo.Samples) %>% 
  mutate(Order = 1:length(SampleID)) %>%
  #mutate(Order = c(12,1489,1509,1556,1580,1600)) %>% 
  ggplot(aes(x = Order, y = dCp)) + geom_point()

### Patient M098 ###
Thermo.Samples <- ClinDemo %>% filter(PatientStudyID == 'M083') %>% select(`Sample ID`) %>% pull()

MelaThermo.long %>% filter(between(Temperature, 45, 90)) %>% filter(SampleID %in% Thermo.Samples) %>%
  ggplot(aes(x = Temperature, y = dCp)) + geom_line(aes(color = SampleID, group = SampleID)) + 
  geom_vline(xintercept = 70)

MelaThermo.long %>% filter(Temperature == 70) %>% 
  filter(SampleID %in% Thermo.Samples) %>% 
  mutate(Order = 1:length(SampleID)) %>%
  #mutate(Order = c(12,1489,1509,1556,1580,1600)) %>% 
  ggplot(aes(x = Order, y = dCp)) + geom_point()


#### OVERLAY DISEASE STATUS ####

AllStatus <- ClinDemo %>% select(`DiseaseStatus`) %>% unique() %>% pull()

ActiveDisease <- ClinDemo %>% 
  filter(`DiseaseStatus` == 'ActiveDisease') %>% 
  filter(!is.na(`Sample ID`)) %>% pull(`Sample ID`)

ActiveDisease.plot <- MelaThermo.long %>% 
  filter(between(Temperature, 45, 90)) %>% 
  filter(SampleID %in% ActiveDisease)

ggplot(ActiveDisease.plot, aes(x = Temperature, y = dCp)) + 
  geom_line(aes(color = SampleID)) +
  theme(legend.position="none")

Active.mean <- ActiveDisease.plot %>% group_by(Temperature) %>% summarize(Mean.dCp = mean(dCp))

ggplot(ActiveDisease.plot, aes(x = Temperature, y = dCp)) + 
  geom_line(aes(color = SampleID)) +
  theme(legend.position="none") +
  geom_line(data = Active.mean, aes(x = Temperature, y = Mean.dCp), col = 'black', size = 1.25) +
  labs(title = paste(AllStatus[1], 'overlay'))



### CREATE OVERLAY FOR ALL STATUS ####

All.means <- NULL
for(i in 1:length(AllStatus))
{
  ActiveDisease <- ClinDemo %>% 
    filter(`DiseaseStatus` == AllStatus[i]) %>% 
    filter(!is.na(`Sample ID`)) %>% pull(`Sample ID`)
  
  ActiveDisease.plot <- MelaThermo.long %>% 
    filter(between(Temperature, 45, 90)) %>% 
    filter(SampleID %in% ActiveDisease)
  
  Active.mean <- ActiveDisease.plot %>% group_by(Temperature) %>% summarize(Mean.dCp = mean(dCp)) %>% mutate(Status = rep(AllStatus[i], nrow(Active.mean)))
  
  p1 <- ggplot(ActiveDisease.plot, aes(x = Temperature, y = dCp)) + 
    geom_line(aes(color = SampleID)) +
    theme(legend.position="none") +
    geom_line(data = Active.mean, aes(x = Temperature, y = Mean.dCp), col = 'black', size = 1.25) +
    labs(title = paste(AllStatus[i], 'overlay'))
  
  print(p1)
  
  All.means <- rbind(All.means, Active.mean)
}

### No large differences in mean behavior between all status types ###

ggplot(All.means, aes(x = Temperature, y = Mean.dCp)) + 
  geom_line(aes(color = Status))


ClinDemo %>% select(TreatmentStatus) %>% unique()
