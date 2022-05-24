library(tidyverse)
library(readxl)
library(reshape2)
library(ggstatsplot)
library(ggpubr)
library(flexdashboard)
library(plotly)
library(qcc)
library(SixSigma)
library(DiagrammeR)
library(AcceptanceSampling)
library(openxlsx)

###general workflow for now###

#load
df<-read_excel(path="sumv3.xlsx")

match("STDODA2",names(df)) #40
match("STDODH2",names(df)) #47
match("STDDiv1",names(df)) #103
match("STDDiv8",names(df)) #110
match("ODRA",names(df)) #21
match("ODRH",names(df)) #28

#identifying the column location
match("CP1Conc",names(df))
match("CP2Conc",names(df))
match("STDConc",names(df))

#regex match, then replace - needs to be improved
df$test <- sub(pattern = "^.*_MW$", replacement = "MW", x = df$`File Name`)
df$test <- sub(pattern = "^.*_DP$", replacement = "DP", x = df$test)
df$test <- sub(pattern = "^.*_CNM$", replacement = "CNM", x = df$test)
df$test <- sub(pattern = "^.*_CM$", replacement = "CM", x = df$test)
df$test <- sub(pattern = "^.*_CWW$", replacement = "CWW", x = df$test)
df$test <- sub(pattern = "^.*_JH$", replacement = "JH", x = df$test)

#standardizing the column name
colnames(df)[colnames(df)=="test"]<-"Initials"

#see output in .csv file
write_csv(df, "resulted_summary_3.csv")

#way of visualizing outliers
ggbetweenstats(df, Plates, CP1Conc, outlier.tagging=T)

#removing outliers per boxplot data
#in cp1conc
outliers<-boxplot(df$CP1Conc,plot=FALSE)$out
dfo<-df[-which(df$CP1Conc %in% outliers),]

#removed?
ggbetweenstats(dfo, Plates, CP1Conc, outlier.tagging=T)

#in cp2conc
outliers<-boxplot(dfo$CP2Conc,plot=FALSE)$out
dfo<-dfo[-which(dfo$CP2Conc %in% outliers),]

#plot
#CP1 and CP2
dfo %>% tidyr::gather("id", "value", c(57,69)) %>% 
  ggplot(., aes(Plates, value,color=Initials ))+
  geom_jitter()+
  facet_wrap(~batch)

#in STDConc
outliers<-boxplot(df$STDConc,plot=FALSE)$out
dfo<-df[-which(df$STDConc %in% outliers),]

#STDConc
dfo %>% tidyr::gather("id", "value", c(31)) %>% 
  ggplot(., aes(Plates, value,color=Initials ))+
  geom_jitter()+
  facet_wrap(~batch)


#Using QCC package

qcc_shew <- qcc(
    df$CP1Conc,
    type = "xbar.one",
    rules = c(1, 2, 3, 4)
)

q2<- qcc(
  df[1:2500,21:28 ],
  type = "R",
  newdata = df[2500:nrow(df), ],
  rules = c(1, 2, 3, 4)
)

#------------------------------future plans, etc.------------------------------------------

dfo %>% tidyr::gather("id", "value", c(57,69)) %>% 
  ggplot(., aes(Plates, value,color=test6 ))+
  geom_jitter()+
  facet_wrap(~batch)


# Change colnames of all columns
colnames(data) <- c("New_Name1", "New_Name2", "New_Name3")

# Change colnames of some columns
colnames(data)[colnames(data) %in% c("Old_Name1", "Old_Name2")] <- c("New_Name1", "New_Name2")



#filtering
as_tibble(df)
vars<-c("Conc. (IU/mL)")
dff<-df %>% filter (.data[[vars[[1]]]] > 1000)



