#***** ETL START *****#

# IMPORT Packages 
lapply(c("tidyverse","rvest"), library, character.only = TRUE)

# DEFINE Variables
currDate<-Sys.Date()
file_name<-paste0("data_covid_",currDate)
master_file_name<-paste0("data_covid.csv")
data_covid<-tibble()
  
# HTML extract
html_covid_lt_3<-paste0("http://covid19.chennaicorporation.gov.in/covid/positivecases/index_det.jsp?RptID=1")
html_covid_ab_3<-paste0("http://covid19.chennaicorporation.gov.in/covid/positivecases/index_det.jsp?RptID=2")

export_html_covid_lt_3<-read_html(html_covid_lt_3)
export_html_covid_ab_3<-read_html(html_covid_ab_3)

# TABLE extract
data_covid_lt_3<-export_html_covid_lt_3 %>% 
  html_nodes(xpath='/html/body/div[2]/div[1]/div[3]/div/div/div/div/div/div/div/div/table') %>%
  html_table()

data_covid_ab_3<-export_html_covid_ab_3 %>% 
  html_nodes(xpath='/html/body/div[2]/div[1]/div[3]/div/div/div/div/div/div/div/div/table') %>%
  html_table()

# ADD ImportDate
data_covid_lt_3<-cbind(data.frame(data_covid_lt_3),importDate=currDate)
data_covid_lt_3<-tibble(data_covid_lt_3)
data_covid_ab_3<-cbind(data.frame(data_covid_ab_3),importDate=currDate)
data_covid_ab_3<-tibble(data_covid_ab_3)

# UNION dataset
data_covid<-union_all(data_covid_lt_3,data_covid_ab_3) 
data_covid<-data_covid %>% select(Zone,Ward,Area,Location,Street,Cases,importDate) %>% distinct()

# Write a copy 
# write.table(data_covid, master_file_name, append = TRUE, col.names = FALSE)

#***** ETL COMPLETE *****#