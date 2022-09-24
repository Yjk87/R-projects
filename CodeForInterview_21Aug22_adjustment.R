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




###general workflow for now###

#load
df1<-read_excel(path="CGH1.xlsx")
df3<-read_excel(path="CLASS.xlsx")
df4<-read_excel(path="grant.xlsx")
#df2<-read_excel(path="TRAC.xlsx")


#standardizing the column name
colnames(df1)[colnames(df1)=="Collaborating Institution City"]<-"CIC"
colnames(df1)[colnames(df1)=="Collaborating Institution Country_Unclean"]<-"CICU"


#df2hi<-filter(df2, Income=="HIC")

df3lowmid<-filter(df3, `Income group` %in% c("Upper middle income","Low income","Lower middle income"))


df1cleaned<-df1
df1cleaned$CICU_Clean <- df1$CICU
df1cleaned$CICU_Clean <- sub(pattern = "^.*South.*Korea.*$", replacement = "Korea, Rep.", x = df1$CICU)
df1cleaned$CICU_Clean <- sub(pattern = "^.*Korea.*of*$", replacement = "Korea, Rep.", x = df1cleaned$CICU_Clean)
df1cleaned$CICU_Clean <- sub(pattern = "^.*UK$", replacement = "United Kingdom", x = df1cleaned$CICU_Clean)
df1cleaned$CICU_Clean <- sub(pattern = "^.*Great.*Britain$", replacement = "United Kingdom", x = df1cleaned$CICU_Clean)

df1cleaned_lowmid<-filter(df1cleaned, CICU_Clean %in% df3lowmid$Economy)




df1IDA<-filter(df1cleaned_lowmid, CICU_Clean %in% df4$IDA)
df1BLEND<-filter(df1cleaned_lowmid, CICU_Clean %in% df4$blend)
df1IBRD<-filter(df1cleaned_lowmid, CICU_Clean %in% df4$IBRD)

df1IDA$Lending <- "IDA"
df1BLEND$Lending <- "Blend"
df1IBRD$Lending<-"IBRD"

df1fixed<- bind_rows(df1IDA, df1IBRD, df1BLEND)

#18aug22
totdf<-merge(y = df3,x= df1fixed, by.y= "Economy", by.x = "CICU_Clean", all.y=T)

#finalize by removing uncleaned names
df1fixed<-subset(df1fixed, select=-CICU)


#df1fixed$Code<- ifelse(df3$Code == df1fixed$CICU_Clean, df3$Code, df1fixed$Code)

df1fixed$Code<-df3$Code[match(df1fixed$CICU_Clean,df3$Economy,)]
df1fixed$Income<-df3$`Income group`[match(df1fixed$CICU_Clean,df3$Economy,)]
#df1fixed$`Income less than` <- sub(pattern = "^Low.*income$", replacement = "1", x = df1fixed$`Income`)
#df1fixed$`Income less than` <- sub(pattern = "^Upper.*middle.*", replacement = "3", x = df1fixed$`Income less than`)
#df1fixed$`Income less than` <- sub(pattern = "^Lower.*middle.*", replacement = "2", x = df1fixed$`Income less than`)

#standardizing the column name AGAIN
colnames(df1fixed)[colnames(df1fixed)=="CIC"]<-"Collaborating Institution City"
colnames(df1fixed)[colnames(df1fixed)=="CICU_Clean"]<-"Collaborating Institution Country_Cleaned"

colnames(totdf)[colnames(totdf)=="CIC"]<-"Collaborating Institution City"
colnames(totdf)[colnames(totdf)=="CICU_Clean"]<-"Collaborating Institution Country_Cleaned"



#https://kcuilla.github.io/reactablefmtr/articles/reactablefmtr_cookbook.html

#colored cell
df1fixed %>%
  mutate(lending_colors = case_when(
    Lending == 'IDA' ~ '#F5A24B',
    Lending == 'IBRD' ~ '#AF52D5',
    Lending == 'blend' ~ '#4C9B9B',
    TRUE ~ 'grey'
  ),income_colors = case_when(
    `Income` == 'Low income' ~ 'red',
    `Income` == 'Lower middle income' ~ 'orange',
    `Income` == 'Upper middle income' ~ 'yellow',
    TRUE ~ 'grey')) %>%
  reactable(
    defaultPageSize = 200,
    columns = list(
      Lending = colDef(
        cell = color_tiles(
          data = .,
          color_ref = 'lending_colors'
        )
      ),
      "Collaborating Institution Country_Cleaned" = colDef(
        cell=color_tiles(
          data=.,
          colors=viridis::mako(5)
          
          
        )
      ),
      Income = colDef(
        cell=color_tiles(
        data=.,  
        color_ref = 'income_colors'  
        )
      ),
      
      
      lending_colors = colDef(show = FALSE),
      income_colors = colDef(show = F)
      )
  ) %>% 
  add_title(
    title = 'Data for CGH Scenario post analysis'
  ) %>% 
  add_subtitle(
    subtitle = 'Data containing grants with collaborations in with LMICs in fiscal year 2021 after data cleaning/analysis',
    font_size = 20,
    font_color = '#666666',
    margin = reactablefmtr::margin(t=10,r=0,b=15,l=0)
  ) %>% 
  add_source(
    source = 'Author: Yongjun Kwon',
    font_style = 'italic',
    font_weight = 'bold'
  )


#colored cell 2
df3 %>%
  mutate(lending_colors = case_when(
    `Lending category` == 'IDA' ~ 'blue',
    `Lending category` == 'IBRD' ~ 'purple',
    `Lending category` == 'Blend' ~ 'green',
    TRUE ~ 'grey'
  ),income_colors = case_when(
    `Income group` == 'Low income' ~ 'red',
    `Income group` == 'Lower middle income' ~ 'orange',
    `Income group` == 'Upper middle income' ~ 'yellow',
    TRUE ~ 'grey'))%>%

  reactable(
    defaultPageSize = 200,
    columns = list(
      'Lending category' = colDef(
        cell = color_tiles(
          data = .,
          color_ref = 'lending_colors'
        ),
        
      ),
      
      'Income group' = colDef(
        cell = color_tiles(
          data = .,
          color_ref = 'income_colors'
        ),
          ),
      
     lending_colors = colDef(show = FALSE),
     income_colors = colDef(show=F)
     )
  )%>% 
  add_title(
    title = 'Current Classification by income'
  ) %>% 
  add_subtitle(
    subtitle = 'Data acquired from the World Bank website',
    font_size = 20,
    font_color = '#666666',
    margin = reactablefmtr::margin(t=10,r=0,b=15,l=0)
  ) %>% 
  add_source(
    source = 'Author: Yongjun Kwon',
    font_style = 'italic',
    font_weight = 'bold'
  )


#try this for unique number ID
df1fixed_id<- transform(df1fixed, id=match(Code, unique(Code)))


#19Aug22

library(crosstalk)

cgh_data <- totdf %>%
  mutate(lending_colors = case_when(
    `Lending category` == 'IDA' ~ '#222FF0',
    `Lending category` == 'IBRD' ~ '#992DF7',
    `Lending category` == 'Blend' ~ '#2DF741',
    TRUE ~ 'grey'
  ),
  
  income_colors = case_when(
    `Income group` == 'Low income' ~ '#ED5564',
    `Income group` == 'Lower middle income' ~ '#FFCE54',
    `Income group` == 'Upper middle income' ~ '#A0D568',
    `Income group` == 'High income' ~ '#AC92EB',
    TRUE ~ 'grey'),
  
  fiscal_colors = case_when(
    `Fiscal Year` == '2021' ~ '#26B53E',
    TRUE ~ 'grey')
    
    
    
  )


data <- SharedData$new(cgh_data)

bscols(
  widths = c(4, 8),
  list(
    filter_checkbox("Income group", "Income group", data, ~`Income group`, inline = TRUE),
    filter_checkbox("Lending category", "Lending category", data, ~`Lending category`, inline = TRUE),
    filter_checkbox("Fiscal", "Fiscal Year", data, ~`Fiscal Year`, inline = TRUE)
  ),
  reactable(
    data,
    # compact = TRUE,
    defaultPageSize = 50,
    theme = clean(),
    columns = list(
       `Income group` = colDef(
        name = 'Income group',
        maxWidth = 150,
        cell = pill_buttons(
          data = cgh_data,
          color_ref = 'income_colors'
        )
      ),
      `Lending category` = colDef(
        name = 'Lending category',
        maxWidth = 150,
        cell=pill_buttons(
          data=cgh_data,
          color_ref = 'lending_colors'
        
        )
        
        
      ),
      `Fiscal Year`=colDef(
        name = 'Fiscal Year',
        maxWidth = 150,
        cell=pill_buttons(
          data=cgh_data,
          color_ref = 'fiscal_colors'
          
        )
        
        
      )
      
      
      
      ,lending_colors = colDef(show = FALSE)
      ,income_colors = colDef(show = FALSE)
      ,Lending = colDef(show = FALSE)
      ,CICU = colDef(show=FALSE)
      ,fiscal_colors = colDef(show=FALSE)
       
    )) %>%
    add_title(
      title = 'CGH Scenario Data Cleaned',
      margin = reactablefmtr::margin(t=18,r=0,b=0,l=0)
    ) 
)

