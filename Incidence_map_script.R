# Set up and data import

#Here are the libraries we'll need
library(sf)
library(rgeos)
library(GISTools)
library(sp)
library(tmap)
library(tidyverse)

#Here's the shape file import:

load("C:/Users/rshea/Desktop/old computer/TB final/new.taiwan.district.rda")

#this creates a combined city district column

new.taiwan.district$site_id <- paste(new.taiwan.district$COUNTYNAME,
                                     new.taiwan.district$TOWNNAME,
                                     sep = "")

# Here's the population data for each district
district_population <- read_excel("district.population.xlsx")
district_population$test <- district_population$site_id

#This adds population data onto the shape file 

district_population$test <- district_population$site_id
Taiwan.districts <- left_join(new.taiwan.district, district_population)

#convert population to number
Taiwan.districts$people_total <- as.numeric(Taiwan.districts$people_total)


# Covid cases file
Covid_cases <- read_excel("C:/Users/rshea/Downloads/Covid cases.xlsx")
Covid_cases$site_id <- paste(Covid_cases$縣市,
                             Covid_cases$區域,
                             sep="")

Covid_cases$testtwo <- Covid_cases$site_id

#add covid to taiwan and calculate incidence

taiwan.covid <- Taiwan.districts %>% left_join(Covid_cases)

## Change NAs to zeros
taiwan.covid[is.na(taiwan.covid$Case.count),]$Case.count <- 0

#incidence

taiwan.covid$incidence <- taiwan.covid$Case.count/taiwan.covid$people_total*100000

# rearrange column order

taiwan.covid <- taiwan.covid %>% relocate(site_id,.before = value)

#maps
incidence.map <-tm_shape(taiwan.covid)+
  tm_borders()+
  tm_shape(taiwan.covid[taiwan.covid$incidence>0,])+
  tm_polygons(col = "incidence", 
            breaks=c(0,10,20,30,40,50,60,70,212),
            title = "盛行率",
            labels = c("0 to 10",
                       "10 to 20",
                       "20 to 30",
                       "30 to 40",
                       "40 to 50",
                       "50 to 60",
                       "60 to 70",
                       "70 to 211"))+
  tm_layout(frame = FALSE, 
            title = "Covid本土個案各鄉鎮市區盛行率今年迄5/22", 
            #main.title.size = 4, 
            #legend.position = c(0.05,0.45), 
            #legend.title.size = 0.0001, 
            #title.position = c(0.05, .9),
            inner.margins=c(0,0,0,0), 
            outer.margins = c(0,0,0,0))

case.count.map <-tm_shape(taiwan.covid)+
  tm_borders()+
  tm_shape(taiwan.covid[taiwan.covid$Case.count>0,])+
  tm_polygons(col = "Case.count",
              breaks = c(seq(0,400,20),406))+
  tm_layout(frame = FALSE, 
            title = "Incidence by district\n01/01/2021 to 05/22/2021", 
            #main.title.size = 4, 
            #legend.position = c(0.05,0.45), 
            #legend.title.size = 0.0001, 
            #title.position = c(0.05, .9),
            inner.margins=c(0,0,0,0), 
            outer.margins = c(0,0,0,0))

# this makes the markdown file

library(rmarkdown)
render("incidence_map_markdown.Rmd")


### Unused code ###

# this is the drop index for islands
#island.district.drop.index <- c("Z","W","X")

#no.islands <- new.taiwan.district[new.taiwan.district$COUNTYID!="Z" &
                                    #new.taiwan.district$COUNTYID!="W" &  
                                    #new.taiwan.district$COUNTYID!="X", 
#]


# This makes a list of counties and their county codes
#listie <- st_drop_geometry(new.taiwan.district[,c("COUNTYNAME","COUNTYID")])
#listie <- cbind(unique(listie$COUNTYNAME),unique(listie$COUNTYID)) 



