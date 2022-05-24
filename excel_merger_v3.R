library(tidyverse)
library(readxl)
library(dplyr)
library(openxlsx)

#set your working directory (somewhere in the odrive)
#setwd("o:/dir1/dir2/dir3")

#read excel files (change pattern accordingly)

files <- list.files(
  #path likely not required at this point. didnt delete
  #path = "./xlsx", 
  
  pattern = "^.*PVA1.*\\.xls$", 
  full.names = T
)

#post naming
merge_fname <- "testing_merged_file.xlsx"


combined.now <- lapply(files, read_excel,range=cell_rows(177:224),col_names = T)

#binding multiple dfs by row (need dplyr) or column? test it please (05/19/2022)
#dplyr bug here, dont use below
#merged.now<-bind_rows(combined.now)

#instead,
merged.now <- do.call("rbind", combined.now)

#end product
write.xlsx(merged.now,merge_fname)
