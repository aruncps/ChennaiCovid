ggplot(All_Chennai_covid_data,aes(x=importDate,y=impactedStreets,group="Chennai")) +
  geom_point()+
  geom_line() +
  geom_smooth(method = "lm", se = TRUE, level=0.95)  


ggplot(Zone_Chennai_covid_data,aes(x=importDate,y=impactedStreets, group=Zone)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, level=0.95) +
  facet_wrap(~Zone)

ggplot(Area_Chennai_covid_data ,aes(x=importDate,y=impactedStreets, group=Area)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, level=0.95) +
  facet_wrap(~Area)

ggplot(Location_Chennai_covid_data %>% filter(Zone==12, Area %in% c('NANGANALLUR','PALAVANTHANGAL')),aes(x=importDate,y=impactedStreets, group=Location)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, level=0.95) +
  facet_wrap(~Location)


ggplot(Street_Chennai_covid_data %>% filter(Zone==12, Area %in% c('NANGANALLUR','PALAVANTHANGAL')),aes(x=importDate,y=impactedStreets, group=Street)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, level=0.95) +
  facet_wrap(~Street, nrow=15)
