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

###general workflow for now###

#load
df<-read_excel(path="summ_new1.xlsx")

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


#standardizing the column name
colnames(df)[colnames(df)=="test"]<-"Initials_1"
colnames(df)[colnames(df)=="test2"]<-"Initials_2"

#arrange by batch number first
df<-df%>%arrange(batch)

#adding sequential numbers
#this doesnt work at the moment
#dfa %>% mutate(ID=row_number())

#this works
df$ID<-1:nrow(df)

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

#----------------------------------------per request--------------------

ggplot(data = df) + 
  geom_boxplot(mapping = aes(y = CP1Conc, x = Initials_1)) +
  geom_boxplot(mapping = aes(y = CP2Conc, x = Initials_2))


ggplot(data = dfo) + 
  geom_boxplot(mapping = aes(x = material, y = CP1Conc, color= Initials)) +
  geom_boxplot(mapping = aes(x = material, y = CP2Conc, color= Initials,)) +
  facet_grid(cols = vars(Initials))


number <- c(12,22,11,26,10,20,21,18)

gender <- c("Male","Female","Male","Female",
            "Female","Male","Female","Male")

friend_or_not <- c("Unknown","Friend","Unknown",
                   "Friend","Unknown","Friend",
                   "Unknown","Friend")

ggplot(df, aes(Initials, number, fill = friend_or_not)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title="Multiple Bar plots")
library(G)


testg1<-ggplot(data = dfo) + 
  +     geom_boxplot(mapping = aes(x = CP1Conc, y = Initials, color= Initials))
testg2<-ggplot(data = dfo) + 
  +     geom_boxplot(mapping = aes(x = CP2Conc, y = Initials, color= Initials))
ggarrange(testg1, testg2, 
          +           labels = c("CP1Conc Range", "CP2Conc Range"),
          +           ncol = 1, nrow = 2)



testg1<-ggplot(data = dfo,aes(x=Initials,y=CP1Conc,color=Initials)) + geom_boxplot()+stat_summary(fun=mean, geom="point",shape=9, size=3)

testg2<-ggplot(data = dfo,aes(x=Initials,y=CP2Conc,color=Initials)) + geom_boxplot()+stat_summary(fun=mean, geom="point",shape=9, size=3)

ggarrange(testg1, testg2, labels = c("CP1Conc Range", "CP2Conc Range"), ncol = 1, nrow = 2)


ggarrange(testg1, testg2, labels= c("CP1Conc Range","CP2Conc Range"), ncol =2, nrow=1)


#post suggestion - 05/27/2022
ggplot(data = dfo) + 
 geom_boxplot(mapping = aes(y = CP1Conc, x = Initials_1, color=Initials_1)) +
 geom_boxplot(mapping = aes(y = CP2Conc, x = Initials_2, color=Initials_1))

 ggplot(dfo,aes(Seq,STDConc))+geom_line()+geom_smooth(method="lm")

 
#the best version currently
 ggplot(data = df) + 
geom_boxplot(mapping = aes(y = CP1Conc, x = Initials_1, color=Initials_1)) +
geom_boxplot(mapping = aes(y = CP2Conc, x = Initials_2, color=Initials_1)) + 
labs(title="CP1 Concentration and CP2 Concentration Variance between Operators",subtitle="Yongjun Kwon, 02Jun22") + xlab("Operator Initials") + ylab("CP Concentration")
 
STDODA2sd<-sd(df$STDODA2)
CP1m<-mean(df$CP1Conc)
CP2m<-mean(df$CP2Conc)
CP1sd<-sd(df$CP1Conc)
CP2sd<-sd(df$CP2Conc)
STDm<-mean(df$STDODA2)
STDsd<-sd(df$STDODA2)
 
 
 
 ggplot(dfa,aes(ID,STDODA2))+geom_jitter(aes(color=Initials_1), size=2)+geom_line()+ labs(title="STDODA2 Variances between Different Operators as of 31May22",subtitle="Yongjun Kwon, 31May22") + xlab("Sequential ID") + ylab("STDODA2 Value") 
 
 
 ggplot(dfa,aes(ID,STDODA2))+geom_line()+geom_smooth(method="glm")
 

 ggplot(data = df) + 
   geom_boxplot(mapping = aes(y = STDConc, x = batch, color=batch))
 
 
 plot_ly(dfa,x=~ID) %>% add_trace(y=~STDODA2, type='scatter',mode='lines+markers',transforms = list(
        type = 'groupby',
     groups = dfa$Initials_1,
     styles = list(
       list(target = 'DP', value = list(marker =list(color = 'blue'))),
       list(target = 'MW', value = list(marker =list(color = 'red'))),
       list(target = 'CNM', value = list(marker =list(color = 'yellow'))),
       list(target = 'CM', value = list(marker =list(color = 'green'))),
       list(target = 'JH', value = list(marker =list(color = 'orange'))),
       list(target = 'CWW', value = list(marker =list(color = 'purple'))),
       list(target = 'YJK', value = list(marker =list(color = 'black')))
     )
   ))
 
 
#--------------------------------------------------------------- new --------------------------

 ggplot(data = df) + 
   geom_boxplot(mapping = aes(y = CP1Conc, x = Initials_1, color=Initials_1)) +
   geom_boxplot(mapping = aes(y = CP2Conc, x = Initials_2, color=Initials_1)) + 
   geom_hline(aes(yintercept= 350, colour="green"))+
   geom_text(aes(0,350,label ="Cutoff",hjust=-.25,vjust = -1))+
   geom_hline(aes(yintercept = 550, colour="green"))+
   geom_text(aes(0,550,label ="Cutoff",hjust=-.25,vjust = -1))+
   labs(title="CP1 Concentration and CP2 Concentration Variance between Operators",subtitle="Yongjun Kwon, 04Jun22") + xlab("Operator Initials") + ylab("CP Concentration")
 
 STDODA2sd<-sd(df$STDODA2)
 CP1m<-mean(df$CP1Conc)
 CP2m<-mean(df$CP2Conc)
 CP1sd<-sd(df$CP1Conc)
 CP2sd<-sd(df$CP2Conc)
 STDm<-mean(df$STDODA2)
 STDsd<-sd(df$STDODA2)
 
 ggplot(df,aes(ID,STDODA2))+
   geom_jitter(aes(color=Initials_1), size=2)+
   geom_line()+
   geom_hline(aes(yintercept= STDm+STDsd, colour="green"))+
   geom_text(aes(0,STDm+STDsd,label ="1 sig",hjust=-.25,vjust = -1))+
   geom_hline(aes(yintercept = STDm-STDsd, colour="green"))+
   geom_text(aes(0,STDm-STDsd,label ="1 sig",hjust=-.25,vjust = -1))+
   geom_hline(aes(yintercept= STDm+(2*STDsd), colour="yellow"))+
   geom_text(aes(0,STDm+(2*STDsd),label ="2 sig",hjust=-.25,vjust = -1))+
   geom_hline(aes(yintercept= STDm-(2*STDsd), colour="yellow"))+
   geom_text(aes(0,STDm-(2*STDsd),label ="2 sig",hjust=-.25,vjust = -1))+
   geom_hline(aes(yintercept= STDm+(3*STDsd), colour="red"))+
   geom_text(aes(0,STDm+(3*STDsd),label ="3 sig",hjust=-.25,vjust = -1))+
   geom_hline(aes(yintercept= STDm-(3*STDsd), colour="red"))+
   geom_text(aes(0,STDm-(3*STDsd),label ="3 sig",hjust=-.25,vjust = -1))+
   geom_smooth(method="glm")+
   labs(title="STDODA2 Variances between Different Operators as of 04Jun22",subtitle="Yongjun Kwon, 04Jun22")+
   xlab("Sequential ID") +
   ylab("STDODA2 Value") 
 
 
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
   labs(title="STDODA2 Variances between Different Operators as of 04Jun22",subtitle="Yongjun Kwon, 04Jun22")+
   xlab("Sequential ID") +
   ylab("STDODA2 Value") 
 