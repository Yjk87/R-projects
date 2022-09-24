#simple regex excercise for generating the Feinstein table

library(tidyverse)
library(readxl)

feinstein<-read_excel(path="Feinstein_YJK.xlsx" , .name_repair = "minimal")

#--------------------------------------------------------------------------------
#first filter
gsub("^Task (\\d).*\\;Task (\\d).*$","\\1, \\2", x = feinstein[1,4])

#second filter
tstreg<-gsub("^Task (\\d).*\\;Task (\\d).*$","\\1, \\2", x = feinstein[2,4])
gsub("^Task (\\d).*","\\1", x = tstreg)

#--------------------------------------------------------------------------------

feinstein$Tracks <- gsub("^Task (\\d).*\\;Task (\\d).*$","\\1, \\2", x = feinstein$`All Tracks Enrolled`)
feinstein$Tracks <- gsub("^Task (\\d).*","\\1", x = feinstein$Tracks)
#feinstein$Tracks
#write_csv(feinstein, "test_feinstein2.csv")

#match("Duration from index",names(feinstein))
#getnum<-which(colnames(feinstein) == "Duration from index") 
#getnum
dim(feinstein)



cohort_feinstein <- feinstein[,-(2:43), drop=FALSE]




sel.feins<- feinstein[,c(1,7,12,17,22,27,32,37)]
dim(sel.feins)
i<-c(2:8)
i
sel.feins[,i]<-lapply(sel.feins[,i], function(x) as.character(x))
feinstein_long <- sel.feins %>% pivot_longer(!`Participant ID`, names_to='Duration',
                                             values_to='Days')

write_csv(feinstein_long, "feinstein_dur.csv")


#+1 visit id (cohort)
i<-1
id.feins<- feinstein[,c(1,7+i,12+i,17+i,22+i,27+i,32+i,37+i)]
i<-c(2:8)
id.feins[,i]<-lapply(id.feins[,i], function(x) as.character(x))
feinstein_long_id <- id.feins %>% pivot_longer(!`Participant ID`, names_to='Visit_ID',
                                             values_to='Cohorts')
#feinstein_long_id %>%
#  group_by(`Visit_ID`) %>%
#  mutate(Instance = cumsum(replace(`Participant ID`, is.na(`Participant ID`),'') == 'TRUE'),
#         Instance2 = cumsum(replace(`Participant ID`, is.na(`Participant ID`),'') == 'FALSE'))
#
#feinstein_long_id$instances<-rowSums(feinstein_long_id == feinstein_long_id$`Participant ID`)
#
#tst<-feinstein_long_id %>% add_count(Visit_ID, `Participant ID`)


feinstein_long_id_final<-feinstein_long_id %>% group_by(`Participant ID`) %>% mutate(instances = row_number())

write_csv(feinstein_long_id_final, "feinstein_id.csv")

#+2 vol of serum collected
i<-2
vol.feins<- feinstein[,c(1,7+i,12+i,17+i,22+i,27+i,32+i,37+i)]
i<-c(2:8)
vol.feins[,i]<-lapply(vol.feins[,i], function(x) as.character(x))
feinstein_long_vol <- vol.feins %>% pivot_longer(!`Participant ID`, names_to='Volume_of_serum_collected(mL)',
                                             values_to='mL')
write_csv(feinstein_long_vol, "feinstein_serum.csv")

#+3 pbmc conc per ml
i<-3
pbmc.feins<- feinstein[,c(1,7+i,12+i,17+i,22+i,27+i,32+i,37+i)]
i<-c(2:8)
pbmc.feins[,i]<-lapply(pbmc.feins[,i], function(x) as.character(x))
feinstein_long_pbmc <- pbmc.feins %>% pivot_longer(!`Participant ID`, names_to='PBMC_Conc_per_mL',
                                             values_to='cells_per_mL')

feinstein_long_pbmc<-na.omit(feinstein_long_pbmc)

as.numeric(unlist(feinstein_long_pbmc[,3]))

mutate(feinstein_long_pbmc, mount_sinai_conc = cells_per_mL/1000000)
feinstein_long_pbmc$newconc <- as.numeric(feinstein_long_pbmc$cells_per_mL) / 1000000

write_csv(feinstein_long_pbmc, "feinstein_pbmc.csv")

#+4 num of vials
i<-4
vials.feins<- feinstein[,c(1,7+i,12+i,17+i,22+i,27+i,32+i,37+i)]
i<-c(2:8)
vials.feins[,i]<-lapply(vials.feins[,i], function(x) as.character(x))
feinstein_long_vials <- vials.feins %>% pivot_longer(!`Participant ID`, names_to='Num_of_PBMC_Vials',
                                             values_to='Vials')
write_csv(feinstein_long_vials, "feinstein_vials.csv")


#join

f_long_id<-inner_join(feinstein_long_id,cohort_feinstein,by=`Participant ID`)


f_long_id<- feinstein_long_id %>% left_join(y=cohort_feinstein, by="Participant ID")
write_csv(f_long_id, "feinstein_combined_cohorts.csv")


feinstein_final<-read_excel(path="S_F_data.xlsx" , .name_repair = "minimal")
