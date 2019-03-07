library(shiny)
library(tidyverse)
library(ggmap)
library(sf)
library(shinydashboard)
library(readr)

# Silly functions to easily convert between years <-> congress numbers
year_to_congress = function(year){
  floor((year - 1789)/2 + 1)
}
congress_to_election = function(congress){
  2 * congress + 1786
}

elections = read_csv("Data/house.csv")

ui = dashboardPage(
  dashboardHeader("US House Elections"),
  dashboardSidebar(),
  dashboardBody()
)

server = function(input, output){
  
}

shinyApp(ui, server)