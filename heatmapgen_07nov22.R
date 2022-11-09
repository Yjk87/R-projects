library(tidyverse)
library(readxl)

#importing
df1<-read_excel("troytable1.xlsx")

#cleaning
df1<-df1[-1,]
df1<-df1[-c(149:152),]
colnames(df1)[2]<-"LAB1"

#number format change
df1$`Concordance Correlation Coefficient`<-as.numeric(gsub('%.*]',"",df1$`Concordance Correlation Coefficient`))
df1$`Concordance Correlation Coefficient`<-df1$`Concordance Correlation Coefficient`/100

df1$`Pearson (Precision)`<-as.numeric(gsub('%.*]',"",df1$`Pearson (Precision)`))
df1$`Pearson (Precision)`<-df1$`Pearson (Precision)`/100

df1$`Accuracy Coefficient`<-as.numeric(gsub('%.*]',"",df1$`Accuracy Coefficient`))
df1$`Accuracy Coefficient`<-df1$`Accuracy Coefficient`/100

#using hpv-types as factors
df1<- df1 %>% fill(`HPV-Type`)
vec<-c()
for (i in unique(df1$`HPV-Type`)) {
  # Inside the loop, make one or elements to add to vector
  new_elements <- i
  # Use 'c' to combine the existing vector with the new_elements
  vec <- c(vec, new_elements)
}
df1$`HPV-Type` <- factor(df1$`HPV-Type`, levels=vec)




#facet_wrapping
#Concordance corr
ggplot(df1, aes(LAB1, LAB2)) +
geom_tile(aes(fill = `Concordance Correlation Coefficient`)) + 
  geom_text(aes(label = round(`Concordance Correlation Coefficient`, 2)), size=2) +
  scale_fill_gradientn(limits= c(0,1), colours=c("blue","white","red"))+
  facet_wrap(vars(df1$`HPV-Type`), nrow=3, scales = "free")+
  ggtitle("Concordance Correlation Coefficient of Lab1/Lab2")+
  theme(plot.title = element_text(size=15, face="bold"), 
        axis.text.x = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"))
#Pearson corr
ggplot(df1, aes(LAB1, LAB2)) +
  geom_tile(aes(fill = `Pearson (Precision)`)) + 
  geom_text(aes(label = round(`Pearson (Precision)`, 2)), size=2) +
  scale_fill_gradientn(limits= c(0,1), colours=c("blue","white","red"))+
  facet_wrap(vars(df1$`HPV-Type`), nrow=3, scales = "free")+
  ggtitle("Pearson Coefficient of Lab1/Lab2")+
  theme(plot.title = element_text(size=15, face="bold"), 
        axis.text.x = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"))
#Accuracy
ggplot(df1, aes(LAB1, LAB2)) +
  geom_tile(aes(fill = `Accuracy Coefficient`)) + 
  geom_text(aes(label = round(`Accuracy Coefficient`, 2)), size=2) +
  scale_fill_gradientn(limits= c(0,1), colours=c("blue","white","red"))+
  facet_wrap(vars(df1$`HPV-Type`), nrow=3, scales = "free")+
  ggtitle("Accuracy Coefficient of Lab1/Lab2")+
  theme(plot.title = element_text(size=15, face="bold"), 
        axis.text.x = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"))



#Potentially not needed after facet_grid

######################################## Functions for individual plotting #########################################
plotCCC <- function(data, x, y) {
  ggplot(data, aes({{x}},{{y}})) +
    geom_tile(aes(fill = `Concordance Correlation Coefficient`)) + 
    geom_text(aes(label = round(`Concordance Correlation Coefficient`, 2))) +
    scale_fill_gradientn(limits= c(0,1), colours=c("blue","white","red"))+
    ggtitle("Concordance Correlation Coefficient of Lab1/Lab2")+
    theme(plot.title = element_text(size=15, face="bold"), 
          axis.text.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold"))
}

plotPP <- function(data, x, y) {
  ggplot(data, aes({{x}},{{y}})) +
    geom_tile(aes(fill = `Pearson (Precision)`)) + 
    geom_text(aes(label = round(`Pearson (Precision)`,2))) +
    scale_fill_gradientn(limits= c(0,1), colours=c("blue","white","red"))+
    ggtitle("Pearson Correlation of Lab1/Lab2")+
    theme(plot.title = element_text(size=15, face="bold"), 
          axis.text.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold"))
}

plotAC <- function(data, x, y) {
  ggplot(data, aes({{x}},{{y}})) +
    geom_tile(aes(fill = `Accuracy Coefficient`)) + 
    geom_text(aes(label = round(`Accuracy Coefficient`,2))) +
    scale_fill_gradientn(limits= c(0,1), colours=c("blue","white","red"))+
    ggtitle("Accuracy Coefficient of Lab1/Lab2")+
    theme(plot.title = element_text(size=15, face="bold"), 
          axis.text.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold"))
}
##############################################################################################


#individual plots if you want
plotCCC(df1[df1$`HPV-Type`=="HPV6",], LAB1, LAB2)
plotCCC(df1[df1$`HPV-Type`=="HPV11",], LAB1, LAB2)
plotCCC(df1[df1$`HPV-Type`=="HPV16",], LAB1, LAB2)
plotCCC(df1[df1$`HPV-Type`=="HPV18",], LAB1, LAB2)
plotCCC(df1[df1$`HPV-Type`=="HPV31",], LAB1, LAB2)
plotCCC(df1[df1$`HPV-Type`=="HPV33",], LAB1, LAB2)
plotCCC(df1[df1$`HPV-Type`=="HPV45",], LAB1, LAB2)
plotCCC(df1[df1$`HPV-Type`=="HPV52",], LAB1, LAB2)
plotCCC(df1[df1$`HPV-Type`=="HPV58",], LAB1, LAB2)
plotPP(df1[df1$`HPV-Type`=="HPV6",], LAB1, LAB2)
plotPP(df1[df1$`HPV-Type`=="HPV11",], LAB1, LAB2)
plotPP(df1[df1$`HPV-Type`=="HPV16",], LAB1, LAB2)
plotPP(df1[df1$`HPV-Type`=="HPV18",], LAB1, LAB2)
plotPP(df1[df1$`HPV-Type`=="HPV31",], LAB1, LAB2)
plotPP(df1[df1$`HPV-Type`=="HPV33",], LAB1, LAB2)
plotPP(df1[df1$`HPV-Type`=="HPV45",], LAB1, LAB2)
plotPP(df1[df1$`HPV-Type`=="HPV52",], LAB1, LAB2)
plotPP(df1[df1$`HPV-Type`=="HPV58",], LAB1, LAB2)
plotAC(df1[df1$`HPV-Type`=="HPV6",], LAB1, LAB2)
plotAC(df1[df1$`HPV-Type`=="HPV11",], LAB1, LAB2)
plotAC(df1[df1$`HPV-Type`=="HPV16",], LAB1, LAB2)
plotAC(df1[df1$`HPV-Type`=="HPV18",], LAB1, LAB2)
plotAC(df1[df1$`HPV-Type`=="HPV31",], LAB1, LAB2)
plotAC(df1[df1$`HPV-Type`=="HPV33",], LAB1, LAB2)
plotAC(df1[df1$`HPV-Type`=="HPV45",], LAB1, LAB2)
plotAC(df1[df1$`HPV-Type`=="HPV52",], LAB1, LAB2)
plotAC(df1[df1$`HPV-Type`=="HPV58",], LAB1, LAB2)


#individual - for testing
ggplot(df1[df1$`HPV-Type` == "HPV6",], aes(LAB1, LAB2)) +
  geom_tile(aes(fill = `Concordance Correlation Coefficient`)) + 
  geom_text(aes(label = round(`Concordance Correlation Coefficient`, 2))) +
  scale_fill_gradientn(limits= c(0,1), colours=c("blue","white","red"))+
  ggtitle("Concordance Correlation Coefficient of Lab1/Lab2", subtitle = "HPV6")+
  theme(plot.title = element_text(size=15, face="bold"), 
        axis.text.x = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"))

ggplot(df1[df1$`HPV-Type` == "HPV6",], aes(LAB1, LAB2)) +
  geom_tile(aes(fill = `Pearson (Precision)`)) + 
  geom_text(aes(label = round(`Pearson (Precision)`, 2))) +
  scale_fill_gradientn(limits= c(0,1), colours=c("blue","white","red"))+
  ggtitle("Concordance Correlation Coefficient of Lab1/Lab2", subtitle = "HPV6")+
  theme(plot.title = element_text(size=15, face="bold"), 
        axis.text.x = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"))

ggplot(df1[df1$`HPV-Type` == "HPV6",], aes(LAB1, LAB2)) +
  geom_tile(aes(fill = `Accuracy Coefficient`)) + 
  geom_text(aes(label = round(`Accuracy Coefficient`, 2))) +
  scale_fill_gradientn(limits= c(0,1), colours=c("blue","white","red"))+
  ggtitle("Concordance Correlation Coefficient of Lab1/Lab2", subtitle = "HPV6")+
  theme(plot.title = element_text(size=15, face="bold"), 
        axis.text.x = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"))

