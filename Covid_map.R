#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(sf)
library(tmap)
library(dplyr)


new.covid.ui <- fluidPage(

    # date choose 
    selectizeInput("foo", choices = NULL, label = "Variable to display"),
    dateInput("day", "Pick a day",min = "2021-05-25", max = Sys.Date(), value = Sys.Date()),
    tmapOutput("map")
    )

covid.server <- function(input,output,session){
    load("covid.data.rda")
    load("taiwan.districts.rda")
    
    updateSelectizeInput(session, "foo", choices = c("new_per_capita",
                                                     "total_per_capita",
                                                     "new_cases",
                                                     "total_cases",
                                                     "people_total"), server = TRUE)
   
     shapefile <- reactive({Taiwan.districts %>% 
        left_join(covid.data[covid.data$date==input$day,],
                  by= c("site_id" = "site_id"))%>%
            dplyr::relocate("site_id",.before = value)})
                           
    
    output$map <- renderTmap({
        tm_shape(shapefile())+
            tm_borders()+
            tm_shape(shapefile()[shapefile()$total_cases >0,])+
            tm_fill(col = input$foo)
            
        
    })
}

shinyApp(new.covid.ui,covid.server)



