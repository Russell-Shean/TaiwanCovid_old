#5/28 map script



###############################################################################
####### load previous days work
###############################################################################

source("cumul_527_map_script.R")

rm(new_cases526to527_per_HundGrand.map)
rm(total527_cases_per_HundGrand.map)

###############################################################################

#####                    5/28     ##################

# Covid cases file 5/28
covid_528_cases <- read_excel("covid_528_cases.xlsx")
covid_528_cases$site_id <- paste(covid_528_cases$City,
                                 covid_528_cases$District,
                                 sep="")

#add 5/28 covid to taiwan
taiwan.covid <- taiwan.covid %>% left_join(covid_528_cases, by=c("site_id"="site_id"))
taiwan.covid[is.na(taiwan.covid$cases_528),]$cases_528 <- 0



######################################################
#####   cumulative cases per 100,000
######################################################

#cases_per_HundGrand_528total

taiwan.covid$cases_per_HundGrand_528total <- taiwan.covid$cases_528 /taiwan.covid$people_total*100000


####################################################
###### new from yesterday  ########################
###################################################

#change from 5/27 to 5/28

taiwan.covid$change_527_528 <- taiwan.covid$cases_528 - taiwan.covid$cases_527_5am


#cases_per_hundred_thousand

taiwan.covid$new_cases_527to528_per_HundGrand <- taiwan.covid$change_527_528 /taiwan.covid$people_total*100000


#####################################################
######   mapping  ##################################
#####################################################

#mapping pre-step:
# rearrange column order
taiwan.covid <- taiwan.covid %>% relocate(site_id,.before = value)


#####################################################3
####   New cases 5/27 to 5/28 ######################
###################################################

new_cases527to528_per_HundGrand.map <-tm_shape(taiwan.covid)+
  tm_borders()+
  tm_shape(taiwan.covid[taiwan.covid$new_cases_527to528_per_HundGrand >0,])+
  tm_polygons(col = "new_cases_527to528_per_HundGrand", 
              breaks=c(0,10,20,30,40,50,60),
              title = "New cases per 10^6",
              labels = c("0 to 10",
                         "10 to 20",
                         "20 to 30",
                         "30 to 40",
                         "40 to 50",
                         "50 to 60")) 


#######################################################
#########  total cases 5/28 ########################
###################################################

total528_cases_per_HundGrand.map <-tm_shape(taiwan.covid)+
  tm_borders()+
  tm_shape(taiwan.covid[taiwan.covid$cases_per_HundGrand_528total  >0,])+
  tm_polygons(col = "cases_per_HundGrand_528total", 
              breaks=c(0,10,20,30,40,50,60,70,80,90,100,110,120,560),
              title = "Total cases per 10^6",
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
                         "110 to 120",
                         "120 to 560"))


