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
party_colors <- c("#2E74C0", "#CB454A", "#F0E060")

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
           Republican * as.numeric(Winner != "R") + 
           (Republican - SecondPlaceVotes) * as.numeric(Winner == "R"),
         DemocratWasted = 
           Democrat * as.numeric(Winner != "D") + 
           (Democrat - SecondPlaceVotes) * as.numeric(Winner == "D"))
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
            DemocratWasted = sum(DemocratWasted)) %>% 
  mutate(PercWasted = WastedVotes / Total * 100,
         PercRepublicanWasted = RepublicanWasted / RVotes * 100,
         PercDemocratWasted = DemocratWasted / DVotes * 100 )
# Binning by state and year
elections_state_year$caption = paste0(elections_state_year$R, "R-", elections_state_year$D, "D")
elections_state_year$id = tolower(elections_state_year$State)
# Calculating gradient limits
maxD = max(elections_state_year$D)
maxR = max(elections_state_year$R)
maxO = max(elections_state_year$O)
maxTot = max(elections_state_year$Total)
maxD_R = max(elections_state_year$DemSurplus)
minD_R = min(elections_state_year$DemSurplus)
maxReps = max(elections_state_year$Reps)
D_R = max(abs(maxD_R), abs(minD_R))
maxLosing = max(elections_state_year$LosingVotes)
maxWinning = max(elections_state_year$WinningVotes)
maxExcess = max(elections_state_year$ExcessVotes)
maxWasted = max(elections_state_year$WastedVotes)
maxRWasted = max(elections_state_year$RepublicanWasted)
maxDWasted = max(elections_state_year$DemocratWasted)

# Statebin Coordinates for clicks
statebins_coords = data.frame(
  State = c("Hawaii", "Alaska", 
            "California", "Oregon", "Washington",
            "Arizona", "Utah", "Nevada", "Idaho",
            
            "New Mexico", "Colorado", "Wyoming", "Montana",
            "Texas", "Oklahoma", "Kansas", "Nebraska", "South Dakota", "North Dakota",
            "Louisiana", "Arizona", "Montana", "Iowa", "Minnesota",
            
            "Mississippi", "Tennessee", "Kentucky", "Indiana", "Illinois", "Wisconsin",
            "Alabama", "North Carolina", "West Virginia", "Ohio", "Michigan",
            "Georgia", "South Carolina", "Virginia", "Pennsylvania",
            
            "Florida", "Maryland", "New Jersey", "New York",
            "Delaware", "Connecticut", "Massachusetts", "Vermont",
            "Rhode Island", "New Hampshire", "Maine"),
  x = c(rep(1, 2),
        rep(2, 3),
        rep(3, 4),
        
        rep(4, 4),
        rep(5, 6),
        rep(6, 5),
        
        rep(7, 6),
        rep(8, 5),
        rep(9, 4),
        
        rep(10, 4),
        rep(11, 4),
        rep(12, 3)),
  y = c(8, 7,
        5:3,
        6:3,
        
        6:3,
        8:3,
        7:3,
        
        7:2,
        7:3,
        7:4,
        
        8, 5:3,
        5:2,
        4, 2, 1)
)

# Set up App
ui = dashboardPage(
  
  dashboardHeader(title = "US House Elections"),
  
  dashboardSidebar(
    
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
                             "Other Seats" = "O",
                             "Democrat Seat Lead" = "D-R",
                             "Total Seats" = "D+R",
                             "Total Votes" = "V",
                             "Losing Votes" = "LV",
                             "Winning Votes" = "WiV",
                             "Excess Votes" = "EV",
                             "Wasted Votes" = "WaV",
                             "Total Wasted Votes (%)" = "PWV",
                             "Republican Wasted Votes" = "RWV",
                             "Democrat Wasted Votes" = "DWV",
                             "Republican Wasted Votes (%)" = "RWVP",
                             "Democrat Wasted Votes (%)" = "DWVP"),
                 selected = "D")
  ),
  
  dashboardBody(
    fluidRow(
      box(plotOutput("map", height = 550, click = clickOpts(id = "plot_click")), 
          width = 7, height = 575),
      box(div(style = "overflow-y: scroll", verbatimTextOutput("state_info")),
          height = 575,
          width = 5)
    ),
    
    fluidRow(
      box(title = "District Results",
          width = 12,
          height = 575,
          collapsible = TRUE,
          div(style = "overflow-x: scroll", dataTableOutput("click_info"))
      )
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
    if(input$toPlot == "O"){
      myPlot = statebins_continuous(elections_state_year %>% dplyr::filter(Year == input$election), 
                                    state_col = "State", 
                                    value_col = "O",
                                    text_color = "gray", 
                                    font_size = 12, 
                                    state_border_col = "white",
                                    legend_position = "bottom") +
        scale_fill_continuous(low = "#ffffff", high = party_colors[3], limits = c(0, maxO)) + 
        guides(fill = guide_colorbar(title = "Other Seats", barwidth = 20,
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
    if(input$toPlot == "LV"){
      myPlot = statebins_continuous(elections_state_year %>% dplyr::filter(Year == input$election), 
                                    state_col = "State", 
                                    value_col = "LosingVotes",
                                    text_color = "gray", 
                                    font_size = 12, 
                                    state_border_col = "white",
                                    legend_position = "bottom")+ 
        scale_fill_continuous(label = comma,
                              low = "white", high = "purple",
                              limits = c(NA, maxLosing)) + 
        guides(fill = guide_colorbar(title = "Total Losing \n Votes Cast", barwidth = 20,
                                     label.theme = element_text(angle = 35,
                                                                size = 24,
                                                                vjust = 0.5),
                                     title.theme = element_text(hjust = 0.5,
                                                                size = 24))) + 
        theme(legend.position = "bottom")
    }
    if(input$toPlot == "WiV"){
      myPlot = statebins_continuous(elections_state_year %>% dplyr::filter(Year == input$election), 
                                    state_col = "State", 
                                    value_col = "WinningVotes",
                                    text_color = "gray", 
                                    font_size = 12, 
                                    state_border_col = "white",
                                    legend_position = "bottom")+ 
        scale_fill_continuous(label = comma,
                              low = "white", high = party_colors[3],
                              limits = c(NA, maxWinning)) + 
        guides(fill = guide_colorbar(title = "Total Winning \n Votes Cast", barwidth = 20,
                                     label.theme = element_text(angle = 35,
                                                                size = 24,
                                                                vjust = 0.5),
                                     title.theme = element_text(hjust = 0.5,
                                                                size = 24))) + 
        theme(legend.position = "bottom")
    }
    if(input$toPlot == "EV"){
      myPlot = statebins_continuous(elections_state_year %>% dplyr::filter(Year == input$election), 
                                    state_col = "State", 
                                    value_col = "ExcessVotes",
                                    text_color = "gray", 
                                    font_size = 12, 
                                    state_border_col = "white",
                                    legend_position = "bottom")+ 
        scale_fill_continuous(label = comma,
                              low = "white", high = "purple",
                              limits = c(NA, maxExcess)) + 
        guides(fill = guide_colorbar(title = "Total Excess \n Votes Cast", barwidth = 20,
                                     label.theme = element_text(angle = 35,
                                                                size = 24,
                                                                vjust = 0.5),
                                     title.theme = element_text(hjust = 0.5,
                                                                size = 24))) + 
        theme(legend.position = "bottom")
    }
    if(input$toPlot == "WaV"){
      myPlot = statebins_continuous(elections_state_year %>% dplyr::filter(Year == input$election), 
                                    state_col = "State", 
                                    value_col = "WastedVotes",
                                    text_color = "gray", 
                                    font_size = 12, 
                                    state_border_col = "white",
                                    legend_position = "bottom")+ 
        scale_fill_continuous(label = comma,
                              low = "white", high = "purple",
                              limits = c(NA, maxWasted)) + 
        guides(fill = guide_colorbar(title = "Total Wasted \n Votes Cast", barwidth = 20,
                                     label.theme = element_text(angle = 35,
                                                                size = 24,
                                                                vjust = 0.5),
                                     title.theme = element_text(hjust = 0.5,
                                                                size = 24))) + 
        theme(legend.position = "bottom")
    }
    if(input$toPlot == "RWV"){
      myPlot = statebins_continuous(elections_state_year %>% dplyr::filter(Year == input$election), 
                                    state_col = "State", 
                                    value_col = "RepublicanWasted",
                                    text_color = "gray", 
                                    font_size = 12, 
                                    state_border_col = "white",
                                    legend_position = "bottom")+ 
        scale_fill_continuous(label = comma,
                              low = "white", high = party_colors[2],
                              limits = c(NA, maxRWasted)) + 
        guides(fill = guide_colorbar(title = "Total Wasted \n Rep. Votes", barwidth = 20,
                                     label.theme = element_text(angle = 35,
                                                                size = 24,
                                                                vjust = 0.5),
                                     title.theme = element_text(hjust = 0.5,
                                                                size = 24))) + 
        theme(legend.position = "bottom")
    }
    if(input$toPlot == "DWV"){
      myPlot = statebins_continuous(elections_state_year %>% dplyr::filter(Year == input$election), 
                                    state_col = "State", 
                                    value_col = "DemocratWasted",
                                    text_color = "gray", 
                                    font_size = 12, 
                                    state_border_col = "white",
                                    legend_position = "bottom")+ 
        scale_fill_continuous(label = comma,
                              low = "white", high = party_colors[1],
                              limits = c(NA, maxDWasted)) + 
        guides(fill = guide_colorbar(title = "Total Wasted \n Dem. Votes", barwidth = 20,
                                     label.theme = element_text(angle = 35,
                                                                size = 24,
                                                                vjust = 0.5),
                                     title.theme = element_text(hjust = 0.5,
                                                                size = 24))) + 
        theme(legend.position = "bottom")
    }
    if(input$toPlot == "RWVP"){
      myPlot = statebins_continuous(elections_state_year %>% dplyr::filter(Year == input$election), 
                                    state_col = "State", 
                                    value_col = "PercRepublicanWasted",
                                    text_color = "gray", 
                                    font_size = 12, 
                                    state_border_col = "white",
                                    legend_position = "bottom")+ 
        scale_fill_continuous(label = comma,
                              low = "white", high = party_colors[2],
                              limits = c(0, 100)) + 
        guides(fill = guide_colorbar(title = "% Wasted \n Rep. Votes", barwidth = 20,
                                     label.theme = element_text(size = 24),
                                     title.theme = element_text(size = 24))) + 
        theme(legend.position = "bottom")
    }
    if(input$toPlot == "DWVP"){
      myPlot = statebins_continuous(elections_state_year %>% dplyr::filter(Year == input$election), 
                                    state_col = "State", 
                                    value_col = "PercDemocratWasted",
                                    text_color = "gray", 
                                    font_size = 12, 
                                    state_border_col = "white",
                                    legend_position = "bottom")+ 
        scale_fill_continuous(label = comma,
                              low = "white", high = party_colors[1],
                              limits = c(0, 100)) + 
        guides(fill = guide_colorbar(title = "% Wasted \n Dem. Votes", barwidth = 20,
                                     label.theme = element_text(size = 24),
                                     title.theme = element_text(size = 24))) + 
        theme(legend.position = "bottom")
    }
    if(input$toPlot == "PWV"){
      myPlot = statebins_continuous(elections_state_year %>% dplyr::filter(Year == input$election), 
                                    state_col = "State", 
                                    value_col = "PercWasted",
                                    text_color = "gray", 
                                    font_size = 12, 
                                    state_border_col = "white",
                                    legend_position = "bottom")+ 
        scale_fill_continuous(label = comma,
                              low = "white", high = "purple",
                              limits = c(0, 100)) + 
        guides(fill = guide_colorbar(title = "% Wasted Votes", barwidth = 20,
                                     label.theme = element_text(size = 24),
                                     title.theme = element_text(size = 24))) + 
        theme(legend.position = "bottom")
    }
    myPlot
  })
  
  output$click_info = renderDataTable({
    nearestState = nearPoints(statebins_coords, input$plot_click, xvar = "x", yvar = "y", threshold = 40)[1,1]
    elections %>% 
      filter(State == nearestState,
             Year == input$election)
  },     
  options = list(
    pageLength = 10,
    searching = FALSE
  ))
  
  output$state_info = renderText({
    nearestState = nearPoints(statebins_coords, input$plot_click, xvar = "x", yvar = "y", threshold = 40)[1,1]
    stateInfo = elections_state_year %>%
      filter(State == nearestState,
             Year == input$election)
    paste(nearestState, "\n",
          "Total Votes:", as.character(stateInfo$Total), "\n",
          "% Democrat Votes:", as.character(round(stateInfo$DVotes/stateInfo$Total * 100, 2)), "\n",
          "% Democrat Seats:", as.character(round(stateInfo$D/stateInfo$Reps, 2)), "\n",
          "Ratio:", as.character(stateInfo$Dvotes / stateInfo$Total / stateInfo$D * stateInfo$Reps), "\n",
          "% \'Wasted\' Votes:", as.character(round(stateInfo$PercWasted, 2)))
  })
  
}

shinyApp(ui, server)