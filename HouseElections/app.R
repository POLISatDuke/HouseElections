library(shiny)
library(tidyverse)
library(ggmap)
library(sf)
library(shinydashboard)
library(readr)

districts = load("../Data/shapes.Rdata")
elections = read_csv("Data/house.csv")

ui = dashboardPage(
  dashboardHeader("US House Elections"),
  dashboardSidebar(),
  dashboardBody()
)

server = function(input, output){
  
}

shinyApp(ui, server)