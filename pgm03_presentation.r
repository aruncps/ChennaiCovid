ggplot(All_Chennai_covid_data,aes(x=importDate,y=impactedStreets,group="Chennai")) +
  geom_point()+
  geom_line(color='blue') +
  geom_smooth(method = "lm", se = TRUE, level=0.95)  

ggplot(All_Chennai_covid_data,aes(x=importDate,y=countOfCases,group="Chennai")) +
  geom_point()+
  geom_line(color='blue') 


ggplot(Zone_Chennai_covid_data,aes(x=importDate,y=impactedStreets, group=Zone)) +
  geom_point() +
  geom_line(color='blue') +
  # geom_smooth(method = "lm", se = TRUE, level=0.95) +
  facet_wrap(~Zone)
ggplot(Zone_Chennai_covid_data,aes(x=importDate,y=countOfCases, group=Zone)) +
  geom_point() +
  geom_line(color='blue') +
  # geom_smooth(method = "lm", se = TRUE, level=0.95) +
  facet_wrap(~Zone)

ggplot(Area_Chennai_covid_data %>% filter(Zone==12),aes(x=importDate,y=impactedStreets, group=Area)) +
  geom_point() +
  geom_line(color='blue') +
  # geom_smooth(method = "lm", se = TRUE, level=0.95) +
  facet_wrap(~Area)

ggplot(Area_Chennai_covid_data %>% filter(Zone==12),aes(x=importDate,y=countOfCases, group=Area)) +
  geom_point() +
  geom_line(color='blue') +
  # geom_smooth(method = "lm", se = TRUE, level=0.95) +
  facet_wrap(~Area)


#Areaheatmap <- 
Zone12_Chennai_covid_data <- Area_Chennai_covid_data %>%filter(Zone %in% c(12) ) %>% ungroup()

  ggplot(Zone12_Chennai_covid_data
       ,aes(importDate, Area, fill= impactedStreets)) + 
  geom_tile() +
  facet_grid(rows = vars(Zone), switch = "y", scales = "free_y", space = "free_y") +
  scale_fill_gradient(low="#fffaaf", high="#ff4747") +
  ylab(label = "Area") +
  xlab(label = "Date") +
  ggtitle(label = "Zone 12 COVID Impacted Streets") +
  scale_y_discrete(limits = rev(levels(as.factor(droplevels(Zone12_Chennai_covid_data$Area) ))) ) +
  theme_classic() +
  theme(strip.placement = "outside",
          plot.title = element_text(hjust = 0.5),
          axis.title.y = element_blank(), # Remove y-axis title
          strip.background = element_rect(fill = "#EEEEEE", color = "#FFFFFF")
      )


  
  
  
pp<-ggplotly(p, tooltip="text")

# save the widget
# library(htmlwidgets)
# saveWidget(pp, file=paste0("ggplotlyHeatmap.html"))