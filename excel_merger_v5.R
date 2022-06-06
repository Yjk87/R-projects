library(tidyverse)
library(readxl)
library(dplyr)
library(openxlsx)

#set your working directory (somewhere in the odrive)
#setwd("o:/dir1/dir2/dir3")

#read excel files (change pattern accordingly)

files <- list.files(
  #path for the directory
  path = "-", 
  
  pattern = "^.*PVA1.*\\.xls$", 
  full.names = T
)

#post naming
merge_fname <- "summary06062022.xlsx"


combined.now <- lapply(files, read_excel,range=cell_rows(177:225),col_names = T)


#instead,
merged.now <- do.call("rbind", combined.now)

#end product
write.xlsx(merged.now,merge_fname)
