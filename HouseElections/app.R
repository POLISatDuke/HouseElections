library(shiny)
library(tidyverse)
library(sf)
library(shinydashboard)
library(readr)
library(fiftystater)
library(mapproj)
library(scales)
library(statebins)
library(geosphere)
library(maptools)
library(rgeos)

### Disable Sci Notation
options(scipen = 999)

### Silly functions to easily convert between years <-> congress numbers
year_to_congress = function(year){
  floor((year - 1789)/2)+1
}

# Returns which congress was in session that year, not which was elected that year
# If an election year is input, function will return the # congress voted in that year - 1
congress_to_election = function(congress){
  2 * congress + 1786
}

### Various handy objects
# Hex color codes for Dem Blue and Rep Red
party_colors <- c("#2E74C0", "#CB454A")

# US state boundaries
data("fifty_states")

### Data load
# Removes parsing failures (mistakes in data formatting)
elections = read_csv("./Data/house.csv")
problemrows = problems(elections)$row
if(length(problemrows)>0){
  elections = elections[-problemrows,]
}
# For each district, calculate # of voters who voted for winning candidates, losing candidates, and second-place candidates
# Using these calculate how many kinds of "wasted" votes there are; votes in excess of second place or for losing candidates
elections = elections %>%
  mutate(SecondPlaceVotes = 
           case_when(
             Republican > Democrat & Democrat > Other ~ Republican - Democrat,
             Republican > Other & Other > Democrat ~ Republican - Other,
             Democrat > Republican & Republican > Other ~ Democrat - Republican,
             Democrat > Other & Other > Republican ~ Democrat - Other,
             Other > Democrat & Democrat > Republican ~ Other - Democrat,
             Other > Republican & Republican > Democrat ~ Other - Republican
           ),
         LosingVotes = 
           Republican * as.numeric(Winner != "R") + 
           Democrat * as.numeric(Winner != "D") + 
           Other * as.numeric(Winner != "O"),
         WinningVotes =
           Republican * as.numeric(Winner == "R") +
           Democrat * as.numeric(Winner == "D") +
           Other * as.numeric(Winner == "O"),
         ExcessVotes = WinningVotes - SecondPlaceVotes,
         WastedVotes = ExcessVotes + LosingVotes,
         RepublicanWasted = 
           LosingVotes * as.numeric(Winner != "R") + 
           ExcessVotes * as.numeric(Winner == "R"),
         DemocratWasted = 
           LosingVotes * as.numeric(Winner != "D") + 
           ExcessVotes * as.numeric(Winner == "D"))
elections_state_year = elections %>%
  group_by(State, Year) %>%
  summarize(R = sum(Winner == "R"),
            D = sum(Winner == "D"),
            Total = as.numeric(sum(Total)),
            DemSurplus = D - R,
            Reps = D + R,
            RVotes = sum(Republican),
            DVotes = sum(Democrat),
            OVotes = sum(Other),
            LosingVotes = sum(LosingVotes),
            WinningVotes = sum(WinningVotes),
            ExcessVotes = sum(ExcessVotes),
            WastedVotes = sum(WastedVotes),
            RepublicanWasted = sum(RepublicanWasted),
            DemocratWasted = sum(DemocratWasted)
  ) %>% 
  mutate(PercWasted = WastedVotes / Total * 100,
         PercRepublicanWasted = RepublicanWasted / RVotes * 100,
         PercDemocratWasted = DemocratWasted / DVotes * 100,
         WasteProbRatio = PercDemocratWasted / PercRepublicanWasted)
# Binning by state and year
elections_state_year$caption = paste0(elections_state_year$R, "R-", elections_state_year$D, "D")
elections_state_year$id = tolower(elections_state_year$State)
# Calculating gradient limits
maxD = max(elections_state_year$D)
maxR = max(elections_state_year$R)
maxTot = max(elections_state_year$Total)
maxD_R = max(elections_state_year$DemSurplus)
minD_R = min(elections_state_year$DemSurplus)
maxReps = max(elections_state_year$Reps)
D_R = max(abs(maxD_R), abs(minD_R))
maxRVotes = max(elections_state_year$RVotes)
maxDVotes = max(elections_state_year$DVotes)
maxOVotes = max(elections_state_year$OVotes)
maxLosing = max(elections_state_year$LosingVotes)
maxWinning = max(elections_state_year$WinningVotes)
maxExcess = max(elections_state_year$ExcessVotes)
maxWasted = max(elections_state_year$WastedVotes)
maxRWasted = max(elections_state_year$RepublicanWasted)
maxDWasted = max(elections_state_year$DemocratWasted)

# Set up App

ui = dashboardPage(
  
  dashboardHeader(title = "US House Elections"),
  
  dashboardSidebar(
    
    width = 400,
    
    sliderInput(inputId = "election",
                label = "Election Year ",
                min = min(elections_state_year$Year),
                max = max(elections_state_year$Year),
                value = max(elections_state_year$Year),
                step = 2,
                animate = TRUE,
                width = 380,
                sep = ""),

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
      box(plotOutput("map", height = 800, hover = hoverOpts(id = "plot_hover")),
          verbatimTextOutput("hover_info"), 
          width = 12, height = 850)
    )
  )
  
)

server = function(input, output){
  
  output$map = renderPlot({
    ### Subset and manipulate data for this year
    this_year = elections_state_year %>%
      dplyr::filter(Year == input$election)
    this_year_sp = left_join(fifty_states, this_year)
    
    ### Plotting Options
    if(input$toPlot == "D"){
      myPlot = statebins_continuous(elections_state_year %>% dplyr::filter(Year == input$election), 
                                    state_col = "State", 
                                    value_col = "D",
                                    text_color = "gray", 
                                    font_size = 12, 
                                    state_border_col = "white",
                                    legend_position = "bottom") +
        scale_fill_continuous(low = "#ffffff", high = party_colors[1], limits = c(0, maxD)) + 
        guides(fill = guide_colorbar(title = "Democrat Seats", barwidth = 20,
                                     label.theme = element_text(size = 24),
                                     title.theme = element_text(size = 24)))
    }
    if(input$toPlot == "R"){
      myPlot = statebins_continuous(elections_state_year %>% dplyr::filter(Year == input$election), 
                                    state_col = "State", 
                                    value_col = "R",
                                    text_color = "gray", 
                                    font_size = 12, 
                                    state_border_col = "white",
                                    legend_position = "bottom") +
        scale_fill_continuous(low = "#ffffff", high = party_colors[2], limits = c(0, maxR)) + 
        guides(fill = guide_colorbar(title = "Republican Seats", barwidth = 20,
                                     label.theme = element_text(size = 24),
                                     title.theme = element_text(size = 24)))        
    }
    if(input$toPlot == "D-R"){
      myPlot = statebins_continuous(elections_state_year %>% dplyr::filter(Year == input$election), 
                                    state_col = "State", 
                                    value_col = "DemSurplus",
                                    text_color = "gray", 
                                    font_size = 12, 
                                    state_border_col = "white",
                                    legend_position = "bottom") +
        scale_fill_gradient2(low = party_colors[2], 
                             high = party_colors[1]) +
        guides(fill = guide_colorbar(title = "Democratic Seat Lead", barwidth = 20,
                                     label.theme = element_text(size = 24),
                                     title.theme = element_text(size = 24)))
    }
    if(input$toPlot == "D+R"){
      myPlot = statebins_continuous(elections_state_year %>% dplyr::filter(Year == input$election), 
                                    state_col = "State", 
                                    value_col = "Reps",
                                    text_color = "gray", 
                                    font_size = 12, 
                                    state_border_col = "white",
                                    legend_position = "bottom") + 
        scale_fill_continuous(low = "white", high = "purple",
                              limits = c(NA, maxReps)) + 
        guides(fill = guide_colorbar(title = "Total Representation", barwidth = 20)) +
        theme(legend.position = "bottom",
              legend.text = element_text(size=24),
              legend.title = element_text(size=24))
    }
    if(input$toPlot == "V"){
      myPlot = statebins_continuous(elections_state_year %>% dplyr::filter(Year == input$election), 
                                    state_col = "State", 
                                    value_col = "Total",
                                    text_color = "gray", 
                                    font_size = 12, 
                                    state_border_col = "white",
                                    legend_position = "bottom")+ 
        scale_fill_continuous(label = comma,
                              low = "white", high = "purple",
                              limits = c(NA, maxTot)) + 
        guides(fill = guide_colorbar(title = "Total \n Votes Cast", barwidth = 20,
                                     label.theme = element_text(angle = 35,
                                                                size = 24,
                                                                vjust = 0.5),
                                     title.theme = element_text(hjust = 0.5,
                                                                size = 24))) + 
        theme(legend.position = "bottom")
    }
    myPlot
  })
  
  output$hover_info = renderPrint({
    cat("input$plot_hover:\n")
    str(input$plot_hover)
  })
  
}

shinyApp(ui, server)