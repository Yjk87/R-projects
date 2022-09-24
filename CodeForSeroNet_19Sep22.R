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
df1<-read_excel(path="Biocspec_Serum.xlsx")
df2<-read_excel(path="Biocspec_PBMC_c.xlsx")
#df4<-read_excel(path="grant.xlsx")
#df2<-read_excel(path="TRAC.xlsx")


#1
merged_df<-merge(y = df1,x= df2, by.y= "Research_Participant_ID", by.x = "Research_Participant_ID", all.y=T)
#2
merged_df2<-bind_rows(df1,df2)


colnames(merged_df2)[colnames(merged_df2)=="Biospecimen_Collection_Date_Normalized_Duration"]<-"Duration"

write_csv(merged_df2, "cleaned_summary.csv")







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
testshit<- merged_df2 %>%
  mutate(Type_colors = case_when(
    Type == 'Serum' ~ '#F5A24B',
    Type == 'PBMC' ~ '#AF52D5',
    TRUE ~ 'grey'
  ),Cohort_colors = case_when(
    Cohort == 'IRIS' ~ 'red',
    Cohort == 'MARS' ~ 'orange',
    Cohort == 'TITAN' ~ 'yellow',
    Cohort == 'Transplant' ~ 'green',
    Cohort == 'Cancer' ~ 'blue',
    Cohort == 'HIV' ~ 'purple',
    TRUE ~ 'grey'),CBC_colors = case_when(
      CBC == 'Mount_Sinai' ~ 'red',
      CBC == 'UMN' ~ 'green',
      CBC == 'Feinstein_Northwell' ~ 'orange',
      TRUE ~ 'grey')) %>% 
  reactable(
    defaultPageSize = 200,
    columns = list(
      `Type` = colDef(
        cell = color_tiles(
          data = .,
          color_ref = 'Type_colors'
        )
      ),
     
      Cohort = colDef(
        cell=color_tiles(
        data=.,  
        color_ref = 'Cohort_colors'  
        )
      ),
      
      
      Cohort_colors = colDef(show = FALSE),
      Type_colors = colDef(show = F),
      CBC_colors = colDef(show = F)
      )
  ) %>% 
  add_title(
    title = 'Data for post analysis'
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


#1922

library(crosstalk)

snet_data <- merged_df2 %>%
  mutate(CBC_colors = case_when(
    CBC == 'Mount_Sinai' ~ '#222FF0',
    CBC == 'UMN' ~  '#992DF7',
    CBC == 'Feinstein_Northwell' ~ '#2DF741',
    TRUE ~ 'grey'
  ),
  
  Cohort_colors = case_when(
    Cohort == 'IRIS' ~       '#ED5564',
    Cohort == 'MARS' ~       '#FFCE54',
    Cohort == 'TITAN' ~      '#A0D568',
    Cohort == 'Transplant' ~ '#AC92EB',
    Cohort == 'Cancer' ~     '#222FF0',
    Cohort == 'HIV' ~        '#992DF7',
    TRUE ~ 'grey'),
  
  Type_colors = case_when(
    Type == 'Serum' ~ '#26B53E',
    Type == 'PBMC' ~  '#AF52D5',
    TRUE ~ 'grey')

  )



data <- SharedData$new(snet_data)

bscols(
  widths = c(2, 10),
  list(
    filter_checkbox("CBC", "CBC", data, ~CBC, inline = TRUE),
    filter_checkbox("Cohort", "Cohort", data, ~Cohort, inline = TRUE),
    filter_checkbox("Type", "Type", data, ~Type, inline = TRUE),
    filter_slider("Duration", "Duration", data, ~Duration, round = TRUE, width = "35%")
  ),
  reactable(
    data,
    # compact = TRUE,
    defaultPageSize = 25,
    theme = reactableTheme(
      borderColor = "#dfe2e5",
      stripedColor = "#f6f8fa",
      highlightColor = "#f0f5f9",
      cellPadding = "8px 12px",
      style = list(
        fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"
      )),
    columns = list(
       CBC = colDef(
        name = 'CBC',
        minWidth = 180,
        cell = pill_buttons(
          data = snet_data,
          color_ref = 'CBC_colors'
        )
      ),
      Cohort = colDef(
        name = 'Cohort',
        maxWidth = 150,
        cell=pill_buttons(
          data=snet_data,
          color_ref = 'Cohort_colors'
        
        )
        
        
      ),
      Type=colDef(
        name = 'Type',
        maxWidth = 100,
        cell=pill_buttons(
          data=snet_data,
          color_ref = 'Type_colors'
          
        )
        
        
      ),
     Duration = colDef(
        'Normalized Duration (days)',
        minWidth = 200,
        cell = data_bars(
          data = snet_data, 
          text_position = 'outside-base',
          number_fmt = scales::label_number(accuracy = 0.1), 
          fill_color = viridis::viridis(5),
          animation = 'width 0.4s linear'
        )
      )
      
      
      
      ,CBC_colors = colDef(show = FALSE)
      ,Cohort_colors = colDef(show = FALSE)
      ,Type_colors = colDef(show=FALSE)
     ,Centrifugation_Time = colDef(show=F)
     ,RT_Serum_Clotting_Time=colDef(show=F)
     ,Initial_Volume_of_Biospecimen=colDef(show=F)
     ,Viability_Automated_Count=colDef(show=F)
     ,Storage_Time_in_Mr_Frosty=colDef(show=F)
       
    )) %>%
    add_title(
      title = 'SeroNet Data Table',
      margin = reactablefmtr::margin(t=18,r=0,b=0,l=0)
    ) %>% 
    add_subtitle(
      subtitle = 'Preliminary visualization of biospecimen data',
      font_size = 15,
      font_color = '#666666',
      margin = reactablefmtr::margin(t=10,r=0,b=15,l=0)
    ) %>% 
    add_source(
      source = 'Author: Yongjun Kwon',
      font_style = 'italic',
      font_weight = 'bold'
    ) 
)

