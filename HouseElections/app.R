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

# Removes parsing failures (mistakes in data format)
elections = read_csv("../Data/house.csv")
problemrows = problems(elections)$row
elections = elections[-problemrows,]

ui = dashboardPage(
  
  dashboardHeader(title = "US House Elections"),

  dashboardSidebar(
    
    width = 400,

    sliderInput(inputId = "time",
                label = "Election Year ",
                min = congress_to_election(1),
                max = congress_to_election(116),
                value = congress_to_election(116),
                step = 1,
                animate = TRUE,
                width = 380,
                sep = "")
  ),

  dashboardBody()
  
)

server = function(input, output){
  
}

shinyApp(ui, server)