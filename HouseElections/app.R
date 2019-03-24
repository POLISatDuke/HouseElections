library(shiny)
library(tidyverse)
library(sf)
library(shinydashboard)
library(readr)
library(fiftystater)
library(mapproj)

### Silly functions to easily convert between years <-> congress numbers
year_to_congress = function(year){
  floor((year - 1789)/2)+1
}

# Returns which congress was in session that year, not which was elected that year
# If an election year is input, function will return the # congress voted in that year - 1
congress_to_election = function(congress){
  2 * congress + 1786
}
###

### Various handy objects
# Hex color codes for Dem Blue and Rep Red
party_colors <- c("#2E74C0", "#CB454A") 

# US state boundaries
data("fifty_states")
###

### Data load
# Removes parsing failures (mistakes in data format)
elections = read_csv("../Data/house.csv")
problemrows = problems(elections)$row
elections = elections[-problemrows,]
elections_state_year = elections %>%
  dplyr::group_by(State, Year) %>%
  dplyr::summarize(R = sum(Winner == "R"),
                   D = sum(Winner == "D"),
                   total = as.numeric(sum(Total))) %>%
  dplyr::mutate(demSurplus = D - R)
# Binning to state-years
elections_state_year$caption = paste0(elections_state_year$R, "R-", this_year$D, "D")
elections_state_year$id = tolower(elections_state_year$State)
# Calculating gradient limits
maxD = max(elections_state_year$D)
maxR = max(elections_state_year$R)
maxTot = max(elections_state_year$total)
maxD_R = max(elections_state_year$demSurplus)
minD_R = min(elections_state_year$demSurplus)
D_R = max(abs(maxD_R), abs(minD_R))
###

ui = dashboardPage(

  dashboardHeader(title = "US House Elections"),

  dashboardSidebar(

    width = 400,

    sliderInput(inputId = "election",
                label = "Election Year ",
                min = congress_to_election(1),
                max = congress_to_election(116),
                value = congress_to_election(116),
                step = 2,
                animate = TRUE,
                width = 380,
                sep = ""),

    radioButtons(inputId = "display",
                 label = "Show:",
                 choices = c("States", "Seats"),
                 selected = "States"),
    
    radioButtons(inputId = "toPlot",
                 label = "Color by:",
                 choices = c("Democrat Seats" = "D", 
                             "Republican Seats" = "R",
                             "Democrat Seat Lead" = "D-R",
                             "Total Seats" = "D+R",
                             "Total Votes" = "V"),
                 selected = "D")
  ),

  dashboardBody(
    fluidRow(
      box(plotOutput("map", height = 800), width = 12, height = 850)
    )
  )

)

server = function(input, output){

  output$map = renderPlot({
    ### Subset and manipulate data for this year
    this_year = elections_state_year %>%
      dplyr::filter(Year == input$election)
    this_year_sp = left_join(fifty_states, this_year)
    ###
    
    ### Make base plot first, if statements add options
    if(input$display == "States"){
      myPlot = ggplot(data = this_year_sp,
                          mapping = aes(x = long, y = lat, group = group)) + 
        geom_polygon(color = "black", size = 1/4) + 
        coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
        theme_void()
      
      if(input$toPlot == "D"){
        myPlot = myPlot + aes(fill = D) +
          scale_fill_continuous(low = "#ffffff", high = party_colors[1], limits = c(0, maxD)) + 
          guides(fill = guide_legend(title = "Democrat Seats")) + 
          theme(legend.position = "bottom",
                legend.text = element_text(size = 24),
                legend.title = element_text(size = 24))
      }
      
      if(input$toPlot == "R"){
        myPlot = myPlot + aes(fill = R) +
          scale_fill_continuous(low = "#ffffff", 
                                high = party_colors[2], 
                                limits = c(0, maxR)) + 
          guides(fill = guide_legend(title = "Republican Seats")) + 
          theme(legend.position = "bottom",
                legend.text = element_text(size = 24),
                legend.title = element_text(size = 24))
      }
      
      if(input$toPlot == "D-R"){
        myPlot = myPlot + aes(fill = demSurplus) +
          scale_fill_gradient2(low = party_colors[2], 
                                high = party_colors[1]) + 
          guides(fill = guide_legend(title = "Democrat Seats - Republican Seats")) +
          theme(legend.position = "bottom",
                legend.text = element_text(size = 24),
                legend.title = element_text(size = 24))
      }
      
      if(input$toPlot == "D+R"){
        myPlot = myPlot + aes(fill = D+R) + 
          scale_fill_continuous(low = "white", high = "purple")
      }
      
    }
    if(input$display == "Seats"){
      # Something else
    }

    myPlot
  })

}

shinyApp(ui, server)