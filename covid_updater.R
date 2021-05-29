##############################################
#### 1. load libraries#####################
############################################


covid.date.updater <- function(file){
  #alternative: function(file,today,yesterday)
  
  #note the current wd
  currentwd <- getwd()
  
  #get us to the folder with the covid data
  setwd("C:/Users/rshea/Desktop/old computer/Covid")
  
    
  #import old cases
  load("covid.data.rda")
  
  #import new cases
  require(readxl)
  new.cases <- read_excel(file)
  

  
  #create needed variables
  
  #site_id
  new.cases$site_id <- paste(new.cases$City,
                                   new.cases$District,
                             sep = "")
  
 
  
  #Date
  library(lubridate)
  new.cases$date <- as.Date(Sys.Date())
  #new.cases$date <- as.Date(today)
  
  
  
  #total cases from yesterday
  library(dplyr)
  yesterday <- Sys.Date()-1
  yesterday.cases <- covid.data[covid.data$date==yesterday,]%>%
    dplyr::select(old_total=total_cases,site_id)
  

  
  yesterday_and_today <- left_join(new.cases,
                                   yesterday.cases,
                                   by=c("site_id"="site_id"))
 
  
  #import population data
  district_population <- read_excel("district.population.xlsx")%>%
    dplyr::select(site_id,people_total)
  
  yesterday_and_today <- left_join(yesterday_and_today,
                                   district_population,
                                   by=c("site_id"="site_id"))
  
  
  #Of course numbers aren't numbers...
  yesterday_and_today$people_total <- as.numeric(yesterday_and_today$people_total)
  yesterday_and_today$old_total <- as.numeric(yesterday_and_today$old_total)
  yesterday_and_today$total_cases <- as.numeric(yesterday_and_today$total_cases)

  
  #calculate new cases, incidence and prevalence
  
  #new cases from yesterday
  yesterday_and_today$new_cases <- yesterday_and_today$total_cases - yesterday_and_today$old_total
    
  #incidence 
  yesterday_and_today$new_per_capita <- yesterday_and_today$new_cases/ yesterday_and_today$people_total*100000
  
  #total cases per population
  yesterday_and_today$total_per_capita <- yesterday_and_today$total_cases / yesterday_and_today$people_total*100000
  
  #make sure columns are in correct order for binding
  yesterday_and_today <- yesterday_and_today %>%
    dplyr::select(date,site_id,City,District,new_per_capita,total_per_capita,new_cases,total_cases,people_total)
  
  ################                           
  ###############                                                            
  #works to here...
  #################
  ################
  
  
  new.covid.data <- rbind(covid.data,yesterday_and_today)
  
  
  return(new.covid.data)
}

test.case <- covid.date.updater(file = "covid_526_cases.xlsx",today = "2021-05-26",yesterday = "2021-05-25")
