library(tidyverse)
library(readxl)
library(reshape2)
library(ggstatsplot)
library(ggpubr)
library(stringi)
library(flexdashboard)
library(plotly)
library(reactable)
library(reactablefmtr)
library(htmltools)
library(crosstalk)
#library(xlsx)

sheet_names <- excel_sheets("composite_yj3.xlsx")
list_all <- lapply(sheet_names, function(x) {
  as.data.frame(read_excel("composite_yj3.xlsx",sheet=x))
})

final.df<-list_all %>% reduce (full_join, by="Research_Participant_ID")

wide1<-read_excel("co_hi.xlsx")
wide2<-wide1 %>% pivot_wider(names_from ="Visit_Number", values_from= "Visit_Number")

#define vector
coln_wide <- colnames(wide1)

#remove certain elements from vector
coln_wide<-coln_wide[!coln_wide %in% c("Visit_Number")]

wide2<-pivot_wider(wide1, id_cols = 'Research_Participant_ID', names_from = 'Visit_Number', values_from= coln_wide)

#09Dec22 attempt

temp1<-read_excel("detrepdem.xlsx")
temp2<-read_excel("detrep.xlsx")
temp3<-full_join(temp1, temp2, by="Research_Participant_ID")

temp2<-read_excel("canc.xlsx")
temp4<-full_join(temp3, temp2, by="Research_Participant_ID")

temp2<-read_excel("orgtrans.xlsx")
temp4<-full_join(temp4, temp2, by="Research_Participant_ID")

temp2<-read_excel("blvisit.xlsx")
temp4<-full_join(temp4, temp2, by="Research_Participant_ID")

w