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
library(mgcv)
library(DT)
library(lattice)


###general workflow for now###

#load
df<-read_excel(path="summary06242022.xlsx")


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

#testing regex match
#tstrgx<-c("PVA1_HPV18B11_23May22_CNM")
#this works
#gsub("^PVA1_.*\\d+_(.*)_.*","\\1 \\2",tstrgx)

#regex match, then replace
df$test <- sub(pattern = "^.*_MW$", replacement = "MW1", x = df$`File Name`)
df$test <- sub(pattern = "^.*_DP$", replacement = "DP1", x = df$test)
df$test <- sub(pattern = "^.*_CNM$", replacement = "CNM1", x = df$test)
df$test <- sub(pattern = "^.*_CM$", replacement = "CM1", x = df$test)
df$test <- sub(pattern = "^.*_CWW$", replacement = "CWW1", x = df$test)
df$test <- sub(pattern = "^.*_JH$", replacement = "JH1", x = df$test)
df$test <- sub(pattern = "^.*_YJK$", replacement = "YJK1", x = df$test)
df$test2 <- sub(pattern = "^.*_MW$", replacement = "MW2", x = df$`File Name`)
df$test2 <- sub(pattern = "^.*_DP$", replacement = "DP2", x = df$test2)
df$test2 <- sub(pattern = "^.*_CNM$", replacement = "CNM2", x = df$test2)
df$test2 <- sub(pattern = "^.*_CM$", replacement = "CM2", x = df$test2)
df$test2 <- sub(pattern = "^.*_CWW$", replacement = "CWW2", x = df$test2)
df$test2 <- sub(pattern = "^.*_JH$", replacement = "JH2", x = df$test2)
df$test2 <- sub(pattern = "^.*_YJK$", replacement = "YJK2", x = df$test2)


df$date <- gsub("^PVA1_.*\\d+_(.*)_.*","\\1 \\2", x = df$`File Name`)


#standardizing the column name
colnames(df)[colnames(df)=="test"]<-"Initials_1"
colnames(df)[colnames(df)=="test2"]<-"Initials_2"

#Changing class for the standard columns. (why chr.???)
i<- c(40:47)
df[,i]<-lapply(df[,i], function(x) as.numeric(as.character(x)))

#arrange by batch number first
df<-df%>%arrange(batch)

#adding sequential numbers
#this doesnt work at the moment
#dfa %>% mutate(ID=row_number())

#this works
df$ID<-1:nrow(df)



#remove NAs
df<-na.omit(df)

#see output 
write_csv(df, "cleaned_summary.csv")





#-----------------------Not needed as of 05Jul22----------------------------------------------

#visualizing outliers
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

 
#--------------------------------------------------------------- new --------------------------

 ggplot(data = df) + 
   geom_boxplot(mapping = aes(y = CP1Conc, x = Initials_1, color=Initials_1)) +
   geom_boxplot(mapping = aes(y = CP2Conc, x = Initials_2, color=Initials_1)) + 
   geom_hline(aes(yintercept= 305, colour="green"))+
   geom_text(aes(0,305,label ="Lower Cutoff (305)",hjust=-.25,vjust = -1))+
   geom_hline(aes(yintercept = 507, colour="green"))+
   geom_text(aes(0,507,label ="Upper Cutoff (507)",hjust=-.25,vjust = -1))+
   labs(title="CP1 Concentration and CP2 Concentration Variance between Operators as of 6/24/2022",subtitle="Yongjun Kwon, 24Jun22") + xlab("Operator Initials") + ylab("CP Concentration")
 


 CP1m<-mean(df$CP1Conc)
 CP2m<-mean(df$CP2Conc)
 CP1sd<-sd(df$CP1Conc)
 CP2sd<-sd(df$CP2Conc)
 STDm<-mean(df$STDODA2)
 STDsd<-sd(df$STDODA2)
 
 ggplot(df,aes(ID,STDODA2))+
   geom_jitter(aes(color=Initials_1), size=2)+
   geom_line()+
   geom_hline(aes(yintercept= STDm+STDsd))+
   geom_text(aes(0,STDm+STDsd,label ="1 sig",hjust=-.25,vjust = -1))+
   geom_hline(aes(yintercept = STDm-STDsd))+
   geom_text(aes(0,STDm-STDsd,label ="1 sig",hjust=-.25,vjust = -1))+
   geom_hline(aes(yintercept= STDm+(2*STDsd)))+
   geom_text(aes(0,STDm+(2*STDsd),label ="2 sig",hjust=-.25,vjust = -1))+
   geom_hline(aes(yintercept= STDm-(2*STDsd)))+
   geom_text(aes(0,STDm-(2*STDsd),label ="2 sig",hjust=-.25,vjust = -1))+
   geom_hline(aes(yintercept= STDm+(3*STDsd)))+
   geom_text(aes(0,STDm+(3*STDsd),label ="3 sig",hjust=-.25,vjust = -1))+
   geom_hline(aes(yintercept= STDm-(3*STDsd)))+
   geom_text(aes(0,STDm-(3*STDsd),label ="3 sig",hjust=-.25,vjust = -1))+
   geom_smooth(method="glm")+
   labs(title="STDODA2 Variances between Different Operators as of 24Jun22",subtitle="Yongjun Kwon, 24Jun22")+
   xlab("Date") +
   ylab("STDODA2 Value") 
 
 
 #Just YJK
 YJKSaved<-df[which(df$Initials_1=="YJK1"),]
 STDmYJK<-mean(YJKSaved$STDODA2)
 STDsdYJK<-sd(YJKSaved$STDODA2)

 
  ggplot(YJKSaved,aes(ID,STDODA2))+
   geom_jitter(aes(color=Initials_1), size=2)+
   geom_line()+
   geom_hline(aes(yintercept= STDmYJK+STDsdYJK))+
   geom_text(aes(0,STDmYJK+STDsdYJK,label ="1 sig",hjust=-.25,vjust = -1))+
   geom_hline(aes(yintercept = STDmYJK-STDsdYJK))+
   geom_text(aes(0,STDmYJK-STDsdYJK,label ="1 sig",hjust=-.25,vjust = -1))+
   geom_hline(aes(yintercept= STDmYJK+(2*STDsdYJK)))+
   geom_text(aes(0,STDmYJK+(2*STDsdYJK),label ="2 sig",hjust=-.25,vjust = -1))+
   geom_hline(aes(yintercept= STDmYJK-(2*STDsdYJK)))+
   geom_text(aes(0,STDmYJK-(2*STDsdYJK),label ="2 sig",hjust=-.25,vjust = -1))+
   geom_hline(aes(yintercept= STDmYJK+(3*STDsdYJK)))+
   geom_text(aes(0,STDmYJK+(3*STDsdYJK),label ="3 sig",hjust=-.25,vjust = -1))+
   geom_hline(aes(yintercept= STDmYJK-(3*STDsdYJK)))+
   geom_text(aes(0,STDmYJK-(3*STDsdYJK),label ="3 sig",hjust=-.25,vjust = -1))+
   geom_smooth(method="glm")+
   labs(title="STDODA2 Variances between Different Operators as of 24Jun22",subtitle="Yongjun Kwon, 24Jun22")+
   xlab("Date") +
   ylab("STDODA2 Value") 
 
 
  ggplot(YJKSaved,aes(Plates,STDODA2))+
    geom_jitter(aes(color=Plates), size=2)+
    labs(title="STDODA2 Variances between Different Plates as of 24Jun22",subtitle="Yongjun Kwon, 24Jun22")+
    xlab("Plates") +
    ylab("STDODA2 Value") 
 
  ggplot(YJKSaved,aes(Plates,STDODA2))+
    geom_boxplot(aes(color=Plates))+
    labs(title="STDODA2 Variances between Different Plates as of 24Jun22",subtitle="Yongjun Kwon, 24Jun22")+
    xlab("Plates") +
    ylab("STDODA2 Value")+
    facet_wrap(~Plates)
  
  q<-qcc(YJKSaved$STDODA2, type="xbar.one", std.dev="SD")
  
 
 #Just JH
 JHSaved<-df[which(df$Initials_1=="JH1"),]
 STDmJH<-mean(JHSaved$STDODA2)
 STDsdJH<-sd(JHSaved$STDODA2)
  
 
 
 #Just DP
 DPSaved<-df[which(df$Initials_1=="DP1"),]
 STDmDP<-mean(DPSaved$STDODA2)
 STDsdDP<-sd(DPSaved$STDODA2)
 
 #Adding QC data (Westgard Rules)
 q1=qcc(DPSaved$STDODA2, type = "xbar.one",std.dev = "SD")
 
 
 
 ggplot(DPSaved,aes(Plates,STDODA2))+
   geom_jitter(aes(color=Plates), size=2)+
   labs(title="STDODA2 Variances between Different Plates as of 24Jun22",subtitle="Yongjun Kwon, 24Jun22")+
   xlab("Plates") +
   ylab("STDODA2 Value") 
 
 ggplot(DPSaved,aes(Plates,STDODA2))+
   geom_boxplot(aes(color=Plates))+
   labs(title="STDODA2 Variances between Different Plates as of 24Jun22",subtitle="Yongjun Kwon, 24Jun22")+
   xlab("Plates") +
   ylab("STDODA2 Value")+
   facet_wrap(~Plates)
 
 
 
 
 
 
 #Just CWW
 CWWSaved<-df[which(df$Initials_1=="CWW1"),]
 STDmCWW<-mean(CWWSaved$STDODA2)
 STDsdCWW<-sd(CWWSaved$STDODA2)

 
 
 
 #Just CNM
 CNMSaved<-df[which(df$Initials_1=="CNM1"),]
 STDmCNM<-mean(CNMSaved$STDODA2)
 STDsdCNM<-sd(CNMSaved$STDODA2)

 
 
 
 #Just CM
 CMSaved<-df[which(df$Initials_1=="CM1"),]
 STDmCM<-mean(CMSaved$STDODA2)
 STDsdCM<-sd(CMSaved$STDODA2)
 
 
 
 
 #Just MW
 MWSaved<-df[which(df$Initials_1=="MW1"),]
 STDmMW<-mean(MWSaved$STDODA2)
 STDsdMW<-sd(MWSaved$STDODA2)

 
 
 
 ggplot(df,aes(ID,STDODA2))+
   geom_jitter(aes(color=Initials_1), size=2)+
   geom_line()+
   geom_hline(aes(yintercept= STDm+STDsd))+
   geom_text(aes(0,STDm+STDsd,label ="1 sd",hjust=-.25,vjust = -1))+
   geom_hline(aes(yintercept = STDm-STDsd))+
   geom_text(aes(0,STDm-STDsd,label ="1 sd",hjust=-.25,vjust = -1))+
   geom_hline(aes(yintercept= STDm+(2*STDsd)))+
   geom_text(aes(0,STDm+(2*STDsd),label ="2 sd",hjust=-.25,vjust = -1))+
   geom_hline(aes(yintercept= STDm-(2*STDsd)))+
   geom_text(aes(0,STDm-(2*STDsd),label ="2 sd",hjust=-.25,vjust = -1))+
   geom_hline(aes(yintercept= STDm+(3*STDsd)))+
   geom_text(aes(0,STDm+(3*STDsd),label ="3 sd",hjust=-.25,vjust = -1))+
   geom_hline(aes(yintercept= STDm-(3*STDsd)))+
   geom_text(aes(0,STDm-(3*STDsd),label ="3 sd",hjust=-.25,vjust = -1))+
   geom_smooth(method="glm")+
   labs(title="STDODA2 Variances between Different Operators as of 14Jun22",subtitle="Yongjun Kwon, 14Jun22")+
   xlab("Sequential ID") +
   ylab("STDODA2 Value") 
 
# long data format
 
select.df<- df %>% select(STDODA2,STDODB2, STDODC2, STDODD2, STDODE2, STDODF2, STDODG2, STDODH2, Initials_1, Initials_2, ID, date, Plates)

#selecting STD column pos.
i<- c(1:8)
select.df[,i]<-lapply(select.df[,i], function(x) as.numeric(as.character(x)))

long.select.df<- select.df %>% 
 pivot_longer(
       cols = starts_with("STDOD"),
       names_to = "STD_ID",
       values_to = "OD_Value",
       values_drop_na= T
   )

#adding sequence col.
long.select.df$Seq<-1:nrow(long.select.df)

ggplot(data = long.select.df) + 
  geom_jitter(mapping = aes(y = OD_Value, x = Initials_1, color=Initials_1)) +
  #geom_hline(aes(yintercept= 2, colour="green"))+
  #geom_text(aes(0,2,label ="Lower Cutoff (305)",hjust=-.25,vjust = -1))+
  #geom_hline(aes(yintercept = 4, colour="green"))+
  #geom_text(aes(0,4,label ="Upper Cutoff (507)",hjust=-.25,vjust = -1))+
  labs(title="OD of standard Variance between Operators as of 6/12/2022",subtitle="Yongjun Kwon, 12Jun22") + xlab("Operator Initials") + ylab("CP Concentration")



ggplot(data = long.select.df)+geom_point(aes(STD_ID, OD_Value, colour=ID))+facet_wrap(vars(Initials_1))

#boxplot of serial dilution result between analyst
ggplot(data = long.select.df)+geom_boxplot(aes(STD_ID, OD_Value, colour=Initials_1))+
  geom_hline(aes(yintercept= 4, colour="red"))+
  geom_hline(aes(yintercept= 2.2, colour="red"))+
  geom_hline(aes(yintercept= 0.2, colour="blue"))+
  facet_wrap(vars(Initials_1))+theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

#STDOD value variances per plates
ggplot(data = long.select.df)+geom_jitter(aes(Initials_1, OD_Value, colour=Initials_1))+
    facet_wrap(vars(Plates))+theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))



#making new dataframe per analyst
ggplot(data = long.select.df[which(long.select.df$Initials_1=="JH1"),], aes(x=STD_ID, y=OD_Value))+geom_point(aes(colour=ID))+stat_smooth(method='lm', se=FALSE)
ggplot(data = long.select.df[which(long.select.df$Initials_1=="DP1"),])+geom_point(aes(STD_ID, OD_Value, colour=ID))
ggplot(data = long.select.df[which(long.select.df$Initials_1=="MW1"),])+geom_point(aes(STD_ID, OD_Value, colour=ID))
ggplot(data = long.select.df[which(long.select.df$Initials_1=="CM1"),])+geom_point(aes(STD_ID, OD_Value, colour=ID))
ggplot(data = long.select.df[which(long.select.df$Initials_1=="CNM1"),])+geom_point(aes(STD_ID, OD_Value, colour=ID))
ggplot(data = long.select.df[which(long.select.df$Initials_1=="CWW1"),])+geom_point(aes(STD_ID, OD_Value, colour=ID))
ggplot(data = long.select.df[which(long.select.df$Initials_1=="YJK1"),])+geom_point(aes(STD_ID, OD_Value, colour=ID))

#different way to present
ggplot(data = long.select.df[which(long.select.df$Initials_1=="DP1"),])+geom_point(aes(date, OD_Value, colour=STD_ID))+
  facet_wrap(vars(Plates))+theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
#and so on

#making a new df, with every 8th entry (since all 8 samples will have the same STDOD)
new.select.df<-select.df[seq(1,nrow(df),8),]
head(new.select.df)
#making the new ID row
new.select.df$ID2<-1:nrow(new.select.df)




#RMSE
fitJH<-lm(CP1Conc~ID, data=JHSaved)
rmseJH1<-sqrt(mean(fitJH$residuals^2))
rmseJH1

#min od std1 : 2.2
#max od std1 : 4.0
#max od std8 : 0.2


