#------- Packages
lapply(c("tidyverse","rvest","hrbrthemes","viridis","plotly"), library, character.only = TRUE)
#------- Zone Dictionary
Zone <- data.frame(Zone = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
                   ZoneName = c("Zone 01 - Thiruvottiyur","Zone 02 - Manali"
                                ,"Zone 03 - Madhavaram","Zone 04 - Tondiarpet"
                                ,"Zone 05 - Royapuram","Zone 06 - Thiru. Vi. Ka. Nagar"
                                ,"Zone 07 - Ambattur","Zone 08 - Anna Nagar"
                                ,"Zone 09 - Teynampet","Zone 10 - Kodambakkam"
                                ,"Zone 11 - Valasaravakkam","Zone 12 - Alandur"
                                ,"Zone 13 - Adyar","Zone 14 - Perungudi"
                                ,"Zone 15 - Sholinganallur")
)
#------- Import CSV
Chennai_covid_data<-read.table("/home/arunkumar/Documents/GitHub/ChennaiCovid/data_covid.csv",header=TRUE, row.names=NULL)
Chennai_covid_data<-tibble(Chennai_covid_data)

#------- Zone level Dataframe
Zone_Chennai_covid_data<-Chennai_covid_data %>% 
  group_by(importDate,Zone) %>% 
  summarize(
    impactedStreets=length(Street),
    countOfCases=sum(Cases)
  )
Zone_Chennai_covid_data<- Zone_Chennai_covid_data %>% inner_join(Zone, by = c("Zone"="Zone"))

#------- Area level Dataframe
Area_Chennai_covid_data<-Chennai_covid_data %>% 
  group_by(importDate,Zone,Area) %>% 
  summarize(
    impactedStreets=length(Street),
    countOfCases=sum(Cases)
  )

str(ZoneA_Chennai_covid_data) 

ZoneA_Chennai_covid_data <- Zone_Chennai_covid_data %>% ungroup()
ZoneA_Chennai_covid_data$importDate<-as.Date(as.character(ZoneA_Chennai_covid_data$importDate))

ggplot(Zone0_Chennai_covid_data
       ,aes(importDate, ZoneName, fill= impactedStreets)) + 
  geom_tile(colour="white",size=0.25) +
  scale_fill_gradient(low="#fffaaf", high="#ff4747") +
  ylab(label = "Area") +
  xlab(label = "Date") +
  scale_y_discrete(limits = rev(levels(as.factor(droplevels(Zone0_Chennai_covid_data$ZoneName) ))) 
                   ,position = "left") +
  theme_classic() +
  theme(strip.placement = "outside",
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank(), # Remove y-axis title
        strip.background = element_rect(fill = "#EEEEEE", color = "#FFFFFF"),
        legend.position="top"
  )

ggplot(ZoneA_Chennai_covid_data,aes(importDate, impactedStreets,fill=impactedStreets)) +
  geom_line(aes(importDate, impactedStreets)) +
  geom_point(aes(importDate, impactedStreets),shape = 21, size=2) +
  scale_fill_gradient(low="#fffaaf", high="#ff4747") +
  theme_classic() +
  theme(strip.placement = "outside",
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank(), # Remove y-axis title
        strip.background = element_rect(fill = "#EEEEEE", color = "#FFFFFF"),
        legend.position="top"
  ) +
  facet_wrap(~ ZoneName, scales = "free_x", ncol = 3) 




ggplot(iris, aes(Sepal.Width, Sepal.Length,
                 fill = Petal.Width))+
  geom_point(shape = 21)+
  scale_fill_gradient(low="#fffaaf", high="#ff4747")

  geom_line(aes(cyl, am, fill=NULL), mtcars)

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
Zone_Chennai_covid_data<- Zone_Chennai_covid_data %>% inner_join(Zone, by = c("Zone"="Zone"))
Zone0_Chennai_covid_data <- Zone_Chennai_covid_data %>% ungroup()

ggplot(Zone0_Chennai_covid_data
       ,aes(importDate, ZoneName, fill= impactedStreets)) + 
  geom_tile() +
  scale_fill_gradient(low="#fffaaf", high="#ff4747") +
  ylab(label = "Area") +
  xlab(label = "Date") +
  scale_y_discrete(limits = rev(levels(as.factor(droplevels(Zone0_Chennai_covid_data$ZoneName) ))) 
                   ,position = "right") +
  theme_classic() +
  theme(strip.placement = "outside",
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank(), # Remove y-axis title
        strip.background = element_rect(fill = "#EEEEEE", color = "#FFFFFF"),
        legend.position="top"
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