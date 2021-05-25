# Set up and data import
# working directory
setwd("C:/Users/rshea/Desktop/old computer/Covid")

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
library(readxl)
district_population <- read_excel("C:/Users/rshea/Desktop/old computer/Covid/district.population.xlsx")
district_population$test <- district_population$site_id

#This adds population data onto the shape file 

district_population$test <- district_population$site_id
Taiwan.districts <- left_join(new.taiwan.district, district_population)

#convert population to number
Taiwan.districts$people_total <- as.numeric(Taiwan.districts$people_total)


# Covid cases file
Covid_cases <- read_excel("Covid_cases_district.xlsx")
Covid_cases$site_id <- paste(Covid_cases$City,
                             Covid_cases$District,
                             sep="")

Covid_cases$testtwo <- Covid_cases$site_id

#add covid to taiwan and calculate cases_per_hundred_thousand

taiwan.covid <- Taiwan.districts %>% left_join(Covid_cases)

## Change NAs to zeros
taiwan.covid[is.na(taiwan.covid$cases_525_5am),]$cases_525_5am <- 0

#cases_per_hundred_thousand

taiwan.covid$cases_per_hundred_thousand <- taiwan.covid$cases_525_5am/taiwan.covid$people_total*100000

# rearrange column order

taiwan.covid <- taiwan.covid %>% relocate(site_id,.before = value)


#maps
cases_per_hundred_thousand.map <-tm_shape(taiwan.covid)+
  tm_borders()+
  tm_shape(taiwan.covid[taiwan.covid$cases_per_hundred_thousand>0,])+
  tm_polygons(col = "cases_per_hundred_thousand", 
            breaks=c(0,10,20,30,40,50,60,70,80,90,404),
            title = "Cases per 10^6",
            labels = c("0 to 10",
                       "10 to 20",
                       "20 to 30",
                       "30 to 40",
                       "40 to 50",
                       "50 to 60",
                       "60 to 70",
                       "70 to 80",
                       "80 to 90",
                       "90 to 404"))+
  tm_layout(frame = FALSE, 
            title = "今年迄5/25上午5am各鄉鎮市區每十萬人口的新冠病毒本土個案",
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
            title = "cases_per_hundred_thousand by district\n01/01/2021 to 05/22/2021", 
            #main.title.size = 4, 
            #legend.position = c(0.05,0.45), 
            #legend.title.size = 0.0001, 
            #title.position = c(0.05, .9),
            inner.margins=c(0,0,0,0), 
            outer.margins = c(0,0,0,0))

# this makes the markdown file

#library(rmarkdown)
#render("cases_per_hundred_thousand_map_markdown.Rmd")


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

#Rmarkdown code
# require(prettydoc)

# output: 
#  prettydoc::html_pretty:
# theme: hpstr

# possible way of reducing file size...
# taiwan.covid.smpl <- sf::st_simplify(taiwan.covid, preserveTopology = TRUE, dTolerance = 1000)
