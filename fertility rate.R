#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

#Read in and clean data
data <- read.csv("data2.csv") 
year <- colnames(data)[5:63]
data_new <- 
    data %>%
    gather(year,key = "year",value = "rate")%>%
    select(Series.Name,Country.Name,year,rate)
data_new$year <- as.numeric(str_sub(data_new$year,2,5))
data_new$rate <- as.numeric(data_new$rate) 

data_final <-
    data_new %>%
    filter(Series.Name=="Adolescent fertility rate (births per 1,000 women ages 15-19)")

data_final$year <- as.numeric(data_final$year)
data_final$rate <- as.numeric(data_final$rate)
data_final$Country.Name <- as.character(data_final$Country.Name)

shinydata <- data_final
shinydata <-na.omit(shinydata)

# Define UI
ui <-pageWithSidebar(
    headerPanel("Adolescent Fertility Rate"),
    sidebarPanel(
        selectInput("country", "Country",shinydata$Country.Name)
    ),
    
    mainPanel(
        h3(textOutput("caption")),
        plotOutput("Plot")
    )
)

# Define server 
server <- function(input, output) {
    formulaText <- reactive({
        paste(input$Country & input$Year & input$Rate)
    })
    
    output$caption <- renderText({
        formulaText()
    })
    
    output$Plot <- renderPlot({
        shinydata=filter(shinydata,Country.Name==input$country)
             ggplot(data=shinydata,aes(x=year,y=rate))+geom_point(aes(x=year,y=rate,color=year))
        
    })
}

#Run the app
shinyApp(ui, server)
