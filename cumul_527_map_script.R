##################################################
### load shape files ############################
#####################################################

source("shape_file_script.R")

#####################################################
##### load daily case updates ####################
###################################################

####                    5/22      ##################

# Covid from 5/22
Covid_cases_522 <- read_excel("covid_522_cases.xlsx")
Covid_cases_522$site_id <- paste(Covid_cases_522$City,
                                 Covid_cases_522$District,
                                 sep="")

#add 5/22 covid to taiwan
taiwan.covid <- Taiwan.districts %>% left_join(Covid_cases_522, by= c("site_id" = "site_id") )
taiwan.covid[is.na(taiwan.covid$Case.count),]$Case.count <- 0


####                    5/25      ##################

# Covid cases file 5/25
Covid_cases <- read_excel("Covid_cases_district.xlsx")
Covid_cases$site_id <- paste(Covid_cases$City,
                             Covid_cases$District,
                             sep="")

#add 5/25 covid to taiwan
taiwan.covid <- taiwan.covid %>% left_join(Covid_cases, by=c("site_id"="site_id"))

## Change NAs to zeros
taiwan.covid[is.na(taiwan.covid$cases_525_5am),]$cases_525_5am <- 0



####                    5/26      ##################

# Covid cases file 5/26
covid_526_cases <- read_excel("covid_526_cases.xlsx")
covid_526_cases$site_id <- paste(covid_526_cases$City,
                                 covid_526_cases$District,
                             sep="")

#add 5/26 covid to taiwan
taiwan.covid <- taiwan.covid %>% left_join(covid_526_cases, by=c("site_id"="site_id"))
taiwan.covid[is.na(taiwan.covid$cases_526_5am),]$cases_526_5am <- 0


####                    5/27     ##################

# Covid cases file 5/27
covid_527_cases <- read_excel("covid_527_cases.xlsx")
covid_527_cases$site_id <- paste(covid_527_cases$City,
                                 covid_527_cases$District,
                                 sep="")

#add 5/27 covid to taiwan
taiwan.covid <- taiwan.covid %>% left_join(covid_527_cases, by=c("site_id"="site_id"))
taiwan.covid[is.na(taiwan.covid$cases_527_5am),]$cases_527_5am <- 0



######################################################
#####   cumulative cases per 100,000
######################################################

#cases_per_HundGrand_527total

taiwan.covid$cases_per_HundGrand_527total <- taiwan.covid$cases_527_5am /taiwan.covid$people_total*100000


####################################################
###### new from yesterday  ########################
###################################################

#change from 5/26 to 5/27

taiwan.covid$change_526_527 <- taiwan.covid$cases_527_5am- taiwan.covid$cases_526_5am


#cases_per_hundred_thousand

taiwan.covid$new_cases_526to527_per_HundGrand <- taiwan.covid$change_526_527 /taiwan.covid$people_total*100000


#####################################################
######   mapping  ##################################
#####################################################

#mapping pre-step:
# rearrange column order
taiwan.covid <- taiwan.covid %>% relocate(site_id,.before = value)


#####################################################3
####   New cases 5/26 to 5/27 ######################
###################################################

new_cases526to527_per_HundGrand.map <-tm_shape(taiwan.covid)+
  tm_borders()+
  tm_shape(taiwan.covid[taiwan.covid$new_cases_526to527_per_HundGrand >0,])+
  tm_polygons(col = "new_cases_526to527_per_HundGrand", 
              breaks=c(0,10,20,30,40,50,60),
              title = "New Cases per 10^6",
              labels = c("0 to 10",
                         "10 to 20",
                         "20 to 30",
                         "30 to 40",
                         "40 to 50",
                         "50 to 60")) 
          

#######################################################
#########  total cases 5/27 ########################
###################################################

total527_cases_per_HundGrand.map <-tm_shape(taiwan.covid)+
  tm_borders()+
  tm_shape(taiwan.covid[taiwan.covid$cases_per_HundGrand_527total  >0,])+
  tm_polygons(col = "cases_per_HundGrand_527total", 
              breaks=c(0,10,20,30,40,50,60,70,80,90,100,110,517),
              title = "total Cases per 10^6",
              labels = c("0 to 10",
                         "10 to 20",
                         "20 to 30",
                         "30 to 40",
                         "40 to 50",
                         "50 to 60",
                         "60 to 70",
                         "70 to 80",
                         "80 to 90",
                         "90 to 100",
                         "100 to 110",
                         "110 to 517"))


