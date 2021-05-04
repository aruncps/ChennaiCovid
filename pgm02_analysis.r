
# IMPORT from CSV for further Analysis
Chennai_covid_data<-read.table(master_file_name,header=TRUE, row.names=NULL)
Chennai_covid_data<-tibble(Chennai_covid_data)

All_Chennai_covid_data<-Chennai_covid_data %>% 
  group_by(importDate) %>% 
  summarize(
    impactedStreets=length(Street),
    countOfCases=sum(Cases)
  )

Zone_Chennai_covid_data<-Chennai_covid_data %>% 
  group_by(importDate,Zone) %>% 
  summarize(
    impactedStreets=length(Street),
    countOfCases=sum(Cases)
  )
Area_Chennai_covid_data<-Chennai_covid_data %>% 
  group_by(importDate,Zone,Area) %>% 
  summarize(
    impactedStreets=length(Street),
    countOfCases=sum(Cases)
  )
Location_Chennai_covid_data<-Chennai_covid_data %>% 
  group_by(importDate,Zone,Area,Location) %>% 
  summarize(
    impactedStreets=length(Street),
    countOfCases=sum(Cases)
  )

Street_Chennai_covid_data<-Chennai_covid_data %>% 
  group_by(importDate,Zone,Area,Location,Street) %>% 
  summarize(
    impactedStreets=length(Street),
    countOfCases=sum(Cases)
            )

View(Zone_Chennai_covid_data)
Area_Chennai_covid_data %>% filter(Zone==12)
Location_Chennai_covid_data %>% filter(Zone==12, Area %in% c('NANGANALLUR','PALAVANTHANGAL'))
Street_Chennai_covid_data %>% filter(Zone==12, Area %in% c('NANGANALLUR','PALAVANTHANGAL'))

View(Street_Chennai_covid_data %>% filter(Zone==12, Area %in% c('NANGANALLUR','PALAVANTHANGAL')))