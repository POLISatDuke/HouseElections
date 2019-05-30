library(shiny)
library(readxl)
library(tidyverse)
library(shinydashboard)
library(readr)
library(scales)
library(statebins)

### Disable Scientific Notation
options(scipen = 999)

### Silly functions to easily convert between years <-> congress numbers
year_to_congress = function(year){
  floor((year - 1789)/2)+1
}

# Returns which congress was in session that year, not which was elected that year
# If an election year is input, function will return the # congress voted in (that year - 1)
congress_to_election = function(congress){
  2 * congress + 1786
}

### Various handy objects
# Hex color codes for Dem Blue and Rep Red, Plus a Gold Color for Independents/Etc.
party_colors <- c("#2E74C0", "#CB454A", "#F0E060")

### Data load
# Removes parsing failures (mistakes in data formatting)
elections = read_csv("./Data/house.csv")

# For each district, calculate # of voters who voted for winning candidates, losing candidates, and second-place candidates
# Using these calculate how many kinds of "wasted" votes there are; votes in excess of second place or for losing candidates
elections = elections %>%
  mutate(
    SecondPlaceVotes = 
      case_when(
        Republican > Democrat   & Democrat > Other       ~ Republican - Democrat,
        Republican > Other      & Other > Democrat       ~ Republican - Other,
        Democrat > Republican   & Republican > Other     ~ Democrat - Republican,
        Democrat > Other        & Other > Republican     ~ Democrat - Other,
        Other > Democrat        & Democrat > Republican  ~ Other - Democrat,
        Other > Republican      & Republican > Democrat  ~ Other - Republican,
        TRUE ~ 0
      ),
    LosingVotes = 
      Republican   * as.numeric(Winner != "R") + 
      Democrat     * as.numeric(Winner != "D") + 
      Other        * as.numeric(Winner != "O"),
    LosingPerc = 
      (LosingVotes / Total) * 100,
    WinningVotes =
      Republican   * as.numeric(Winner == "R") +
      Democrat     * as.numeric(Winner == "D") +
      Other        * as.numeric(Winner == "O"),
    WinningPerc = 
      (WinningVotes / Total) * 100,
    ExcessVotes = 
      WinningVotes - SecondPlaceVotes,
    ExcessPerc = 
      WinningPerc - LosingPerc,
    WastedVotes = 
      ExcessVotes + LosingVotes,
    WastedPerc = 
      (WastedVotes / Total) * 100,
    RepublicanWasted = 
      Republican                      * as.numeric(Winner != "R") + 
      (Republican - SecondPlaceVotes) * as.numeric(Winner == "R"),
    RepublicanWastedPerc = 
      RepublicanWasted / Republican * 100,
    DemocratWasted = 
      Democrat                      * as.numeric(Winner != "D") + 
      (Democrat - SecondPlaceVotes) * as.numeric(Winner == "D"),
    DemocratWastedPerc = 
      (DemocratWasted / Democrat) * 100,
    OtherWasted = 
      Other                      * as.numeric(Winner != "O") + 
      (Other - SecondPlaceVotes) * as.numeric(Winner == "O"),
    OtherWastedPerc = 
      OtherWasted / Other * 100,
    DemocratLosing = 
      Democrat * as.numeric(Winner != "D"),
    RepublicanLosing = 
      Republican * as.numeric(Winner != "R"),
    OtherLosing =
      Other * as.numeric(Winner != "O"),
    DemocratWinning = 
      Democrat * as.numeric(Winner == "D"),
    RepublicanWinning = 
      Republican * as.numeric(Winner == "R"),
    OtherWinning =
      Other * as.numeric(Winner == "O"),
    DemocratExcess = 
      (Democrat - SecondPlaceVotes) * as.numeric(Winner == "D"),
    RepublicanExcess = 
      (Republican - SecondPlaceVotes) * as.numeric(Winner == "R"),
    OtherExcess =
      (Other - SecondPlaceVotes) * as.numeric(Winner == "O"))

# Binning by state and year
elections_state_year = elections %>%
  group_by(State, Year) %>%
  summarize(
    R                 = sum(Winner == "R"),
    D                 = sum(Winner == "D"),
    O                 = sum(Winner == "I"),
    total             = as.numeric(sum(Total)),
    DemSurplus        = D - R,
    Reps              = D + R + O,
    RVotes            = sum(Republican),
    DVotes            = sum(Democrat),
    OVotes            = sum(Other),
    LosingVotes       = sum(LosingVotes)/total * 100,
    WinningVotes      = sum(WinningVotes)/total * 100,
    ExcessVotes       = sum(ExcessVotes)/total * 100,
    WastedVotes       = sum(WastedVotes),
    RepublicanWasted  = sum(RepublicanWasted),
    DemocratWasted    = sum(DemocratWasted),
    OtherWasted       = sum(OtherWasted),
    RepublicanLosing  = sum(RepublicanLosing),
    DemocratLosing    = sum(DemocratLosing),
    OtherLosing       = sum(OtherLosing),
    RepublicanWinning = sum(RepublicanWinning),
    DemocratWinning   = sum(DemocratWinning),
    OtherWinning      = sum(OtherWinning),
    DemocratExcess    = sum(DemocratExcess),
    RepublicanExcess  = sum(RepublicanExcess),
    OtherExcess       = sum(OtherExcess)) %>% 
  mutate(
    PercWasted             = WastedVotes / total * 100,
    PercRepublicanWasted   = RepublicanWasted / RVotes * 100,
    PercDemocratWasted     = DemocratWasted / DVotes * 100,
    PercOtherWasted        = OtherWasted / OVotes * 100,
    PercRepublicanLosing   = RepublicanLosing / RVotes * 100,
    PercDemocratLosing     = DemocratLosing / DVotes * 100,
    PercOtherLosing        = OtherLosing / OVotes * 100,
    PercRepublicanWinning  = RepublicanWinning / RVotes * 100,
    PercDemocratWinning    = DemocratWinning / DVotes * 100,
    PercOtherWinning       = OtherWinning / OVotes * 100,
    PercDemocratExcess     = DemocratExcess / DVotes * 100,
    PercRepublicanExcess   = RepublicanExcess/ RVotes * 100,
    PercOtherExcess        = OtherExcess / OVotes * 100)
# Following is not useless line - coerces NaNs to NAs which behave better in statebin
elections_state_year[is.na(elections_state_year)] = NA
elections_state_year = elections_state_year %>% complete(State, Year)
# Complete States/Years combinations - otherwise missing in data means missing in chart


elections_state_year$caption = paste0(elections_state_year$R, "R-", elections_state_year$D, "D")
elections_state_year$id = tolower(elections_state_year$State)

# Statebin Coordinates for clicks
statebins_coords = data.frame(
  State = c(
    "Hawaii", "Alaska", 
    "California", "Oregon", "Washington",
    "Arizona", "Utah", "Nevada", "Idaho",
    
    "New Mexico", "Colorado", "Wyoming", "Montana",
    "Texas", "Oklahoma", "Kansas", "Nebraska", "South Dakota", "North Dakota",
    "Louisiana", "Arkansas", "Missouri", "Iowa", "Minnesota",
    
    "Mississippi", "Tennessee", "Kentucky", "Indiana", "Illinois", "Wisconsin",
    "Alabama", "North Carolina", "West Virginia", "Ohio", "Michigan",
    "Georgia", "South Carolina", "Virginia", "Pennsylvania",
    
    "Florida", "District of Columbia", "Maryland", "New Jersey", "New York",
    "Delaware", "Connecticut", "Massachusetts", "Vermont",
    "Rhode Island", "New Hampshire", "Maine"),
  x = c(
    rep(1, 2),
    rep(2, 3),
    rep(3, 4),
    
    rep(4, 4),
    rep(5, 6),
    rep(6, 5),
    
    rep(7, 6),
    rep(8, 5),
    rep(9, 4),
    
    rep(10, 5),
    rep(11, 4),
    rep(12, 3)),
  y = c(
    8, 7,
    5:3,
    6:3,
    
    6:3,
    8:3,
    7:3,
    
    7:2,
    7:3,
    7:4,
    
    8, 6:3,
    5:2,
    4, 2, 1)
)

# Set up App
ui = dashboardPage(
  
  dashboardHeader(
    title = "U.S. House Elections"
  ),
  
  dashboardSidebar(
    sidebarMenuOutput("sidebar")
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "viz",
        fluidRow(
          box(
            h2("U.S. House Elections"),
            align = "center",
            width = 7, height = 85
          ),
          box(
            imageOutput("polisLogo"),
            align = "center",
            width = 5, height = 85
          )
        ),
        fluidRow(
          box(
            plotOutput("map", height = 550, click = clickOpts(id = "plot_click")), 
            width = 7, height = 575),
          box(
            div(htmlOutput("state_info")),
            width = 5, height = 575)
        ),
        
        fluidRow(
          box(
            title = "District Results",
            width = 12,
            height = 575,
            collapsible = TRUE,
            div(style = "overflow-x: scroll", dataTableOutput("click_info"))
          )
        )
      ),
      
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            h2("About the App"),
            align = "center",
            width = 7, height = 85
          ),
          box(
            imageOutput("polisLogo2"),
            align = "center",
            width = 5, height = 85
          )
        ),
        fluidRow(
          
        )
      )
    )
  )
)

server = function(input, output){
  
  output$polisLogo = renderImage({
    return(list(src = "./polis-logo.jpg", contentType = "image/jpg", alt = "Alignment", width = 300, height = 60))
  }, deleteFile = FALSE)
  
  output$polisLogo2 = renderImage({
    return(list(src = "./polis-logo.jpg", contentType = "image/jpg", alt = "Alignment", width = 300, height = 60))
  }, deleteFile = FALSE)
  
  output$sidebar = renderMenu(
    {
      sidebarMenu(
        id = "sidebarmenu",
        
        menuItem("Representation by State", tabName = "viz", icon = icon("chart-area")),
        
        conditionalPanel(
          "input.sidebarmenu === 'viz'",
          sliderInput(
            inputId = "election",
            label = "Election Year ",
            min = min(elections_state_year$Year),
            max = max(elections_state_year$Year),
            value = max(elections_state_year$Year),
            step = 2,
            animate = TRUE,
            width = 380,
            sep = ""),
          
          radioButtons(
            inputId = "party",
            label = "Major Party:",
            choices = c("Democratic",
                        "Republican",
                        "Independent",
                        "All"),
            selected = "All"),
          
          radioButtons(
            inputId = "toPlot",
            label = "Fill Criterion (%):",
            choices = c("Winning Votes" = "WiV",
                        "Losing Votes" = "LV",
                        "Excess Votes" = "EV",
                        "Wasted Votes" = "WaV"),
            selected = "WiV")
        ),
        
        menuItem("About", tabName = "about", icon = icon("info-circle"))
      )
    }
  )
  
  output$map = renderPlot({
    ### Subset and manipulate data for this year
    this_year = elections_state_year %>%
      dplyr::filter(Year == input$election)
    
    # Color choices: All plots use a single color gradient. 
    # If a major party is selected, gradient is white to their color.
    # All is white to Purple, and Other is white to gold.
    
    
    ### Plotting Options
    if(input$toPlot == "LV" & input$party == "All"){
      myPlot = statebins_continuous(
        elections_state_year %>% dplyr::filter(Year == input$election), 
        state_col = "State", 
        value_col = "LosingVotes",
        text_color = "gray", 
        font_size = 10, 
        state_border_col = "white",
        legend_position = "bottom"
      ) + 
        scale_fill_continuous(
          label = comma,
          low = "white", high = "purple",
          limits = c(0, 100)
        ) +
        guides(
          fill = guide_colorbar(
            title = "Percent Votes \nfor Losing Candidates", barwidth = 20,
            label.theme = element_text(size = 24),
            title.theme = element_text(size = 24))
        ) +
        theme(legend.position = "bottom")
    }
    
    if(input$toPlot == "LV" & input$party == "Democratic"){
      myPlot = statebins_continuous(
        elections_state_year %>% dplyr::filter(Year == input$election), 
        state_col = "State", 
        value_col = "PercDemocratLosing",
        text_color = "gray", 
        font_size = 10, 
        state_border_col = "white",
        legend_position = "bottom"
      ) + 
        scale_fill_continuous(
          label = comma,
          low = "white", high = party_colors[1],
          limits = c(0, 100)
        ) + 
        guides(
          fill = guide_colorbar(title = "Percent Democratic Votes \nfor Losing Candidates", barwidth = 20,
                                label.theme = element_text(size = 24),
                                title.theme = element_text(size = 24))
        ) + 
        theme(legend.position = "bottom")
    }
    if(input$toPlot == "LV" & input$party == "Republican"){
      myPlot = statebins_continuous(
        elections_state_year %>% dplyr::filter(Year == input$election), 
        state_col = "State", 
        value_col = "PercRepublicanLosing",
        text_color = "gray", 
        font_size = 10, 
        state_border_col = "white",
        legend_position = "bottom"
      )+ 
        scale_fill_continuous(
          label = comma,
          low = "white", high = party_colors[2],
          limits = c(0, 100)
        ) + 
        guides(
          fill = guide_colorbar(
            title = "Percent Republican Votes \nfor Losing Candidates", barwidth = 20,
            label.theme = element_text(size = 24),
            title.theme = element_text(size = 24))
        ) + 
        theme(legend.position = "bottom")
    }
    if(input$toPlot == "LV" & input$party == "Independent"){
      myPlot = statebins_continuous(
        elections_state_year %>% dplyr::filter(Year == input$election), 
        state_col = "State", 
        value_col = "PercOtherLosing",
        text_color = "gray", 
        font_size = 10, 
        state_border_col = "white",
        legend_position = "bottom"
      )+ 
        scale_fill_continuous(
          label = comma,
          low = "white", high = party_colors[3],
          limits = c(0, 100)
        ) + 
        guides(
          fill = guide_colorbar(
            title = "Percent Independent Votes \nfor Losing Candidates", barwidth = 20,
            label.theme = element_text(size = 24),
            title.theme = element_text(size = 24))
        ) + 
        theme(legend.position = "bottom")
    }
    if(input$toPlot == "WiV" & input$party == "All"){
      myPlot = statebins_continuous(
        elections_state_year %>% dplyr::filter(Year == input$election), 
        state_col = "State", 
        value_col = "WinningVotes",
        text_color = "gray", 
        font_size = 10, 
        state_border_col = "white",
        legend_position = "bottom"
      ) + 
        scale_fill_continuous(
          label = comma,
          low = "white", high = "purple",
          limits = c(0, 100)
        ) + 
        guides(
          fill = guide_colorbar(
            title = "Percent Votes \nfor Winning Candidates", barwidth = 20,
            label.theme = element_text(size = 24),
            title.theme = element_text(size = 24))
        ) + 
        theme(legend.position = "bottom")
    }
    if(input$toPlot == "WiV" & input$party == "Democratic"){
      myPlot = statebins_continuous(
        elections_state_year %>% dplyr::filter(Year == input$election), 
        state_col = "State", 
        value_col = "PercDemocratWinning",
        text_color = "gray", 
        font_size = 10, 
        state_border_col = "white",
        legend_position = "bottom"
      ) + 
        scale_fill_continuous(
          label = comma,
          low = "white", high = party_colors[1],
          limits = c(0, 100)
        ) + 
        guides(fill = guide_colorbar(
          title = "Percent Democratic Votes \nfor Winning Candidates", barwidth = 20,
          label.theme = element_text(size = 24),
          title.theme = element_text(size = 24))
        ) + 
        theme(legend.position = "bottom")
    }
    if(input$toPlot == "WiV" & input$party == "Republican"){
      myPlot = statebins_continuous(
        elections_state_year %>% dplyr::filter(Year == input$election), 
        state_col = "State", 
        value_col = "PercRepublicanWinning",
        text_color = "gray", 
        font_size = 10, 
        state_border_col = "white",
        legend_position = "bottom"
      ) + 
        scale_fill_continuous(
          label = comma,
          low = "white", high = party_colors[2],
          limits = c(0, 100)
        ) + 
        guides(
          fill = guide_colorbar(
            title = "Percent Republican Votes \nfor Winning Candidates", barwidth = 20,
            label.theme = element_text(size = 24),
            title.theme = element_text(size = 24))
        ) + 
        theme(legend.position = "bottom")
    }
    if(input$toPlot == "WiV" & input$party == "Independent"){
      myPlot = statebins_continuous(
        elections_state_year %>% dplyr::filter(Year == input$election), 
        state_col = "State", 
        value_col = "PercOtherWinning",
        text_color = "gray", 
        font_size = 10, 
        state_border_col = "white",
        legend_position = "bottom"
      ) + 
        scale_fill_continuous(
          label = comma,
          low = "white", high = party_colors[3],
          limits = c(0, 100)
        ) + 
        guides(
          fill = guide_colorbar(
            title = "Percent Independent Votes \nfor Winning Candidates", barwidth = 20,
            label.theme = element_text(size = 24),
            title.theme = element_text(size = 24))
        ) + 
        theme(legend.position = "bottom")
    }
    if(input$toPlot == "EV" & input$party == "All"){
      myPlot = statebins_continuous(
        elections_state_year %>% dplyr::filter(Year == input$election), 
        state_col = "State", 
        value_col = "ExcessVotes",
        text_color = "gray", 
        font_size = 10, 
        state_border_col = "white",
        legend_position = "bottom"
      ) + 
        scale_fill_continuous(
          label = comma,
          low = "white", high = "purple",
          limits = c(0, 100)
        ) + 
        guides(
          fill = guide_colorbar(
            title = "Percent Excess Votes", barwidth = 20,
            label.theme = element_text(size = 24),
            title.theme = element_text(size = 24))
        ) + 
        theme(legend.position = "bottom")
    }
    if(input$toPlot == "EV" & input$party == "Democratic"){
      myPlot = statebins_continuous(
        elections_state_year %>% dplyr::filter(Year == input$election), 
        state_col = "State", 
        value_col = "PercDemocratExcess",
        text_color = "gray", 
        font_size = 10, 
        state_border_col = "white",
        legend_position = "bottom"
      ) + 
        scale_fill_continuous(
          label = comma,
          low = "white", high = party_colors[1],
          limits = c(0, 100)
        ) + 
        guides(
          fill = guide_colorbar(
            title = "Percent of Excess \nDemocratic Votes", barwidth = 20,
            label.theme = element_text(size = 24),
            title.theme = element_text(size = 24))
        ) + 
        theme(legend.position = "bottom")
    }
    if(input$toPlot == "EV" & input$party == "Republican"){
      myPlot = statebins_continuous(
        elections_state_year %>% dplyr::filter(Year == input$election), 
        state_col = "State", 
        value_col = "PercRepublicanExcess",
        text_color = "gray", 
        font_size = 10, 
        state_border_col = "white",
        legend_position = "bottom"
      ) + 
        scale_fill_continuous(
          label = comma,
          low = "white", high = party_colors[2],
          limits = c(0, 100)
        ) + 
        guides(fill = guide_colorbar(
          title = "Percent of Excess \nRepublican Votes", barwidth = 20,
          label.theme = element_text(size = 24),
          title.theme = element_text(size = 24))
        ) + 
        theme(legend.position = "bottom")
    }
    if(input$toPlot == "EV" & input$party == "Independent"){
      myPlot = statebins_continuous(
        elections_state_year %>% dplyr::filter(Year == input$election), 
        state_col = "State", 
        value_col = "PercOtherExcess",
        text_color = "gray", 
        font_size = 10, 
        state_border_col = "white",
        legend_position = "bottom"
      ) + 
        scale_fill_continuous(
          label = comma,
          low = "white", high = party_colors[2],
          limits = c(0, 100)
        ) + 
        guides(fill = guide_colorbar(
          title = "Percent of Excess \nIndependent Votes", barwidth = 20,
          label.theme = element_text(size = 24),
          title.theme = element_text(size = 24))
        ) + 
        theme(legend.position = "bottom")
    }
    if(input$toPlot == "WaV" & input$party == "Democratic"){
      myPlot = statebins_continuous(
        elections_state_year %>% dplyr::filter(Year == input$election), 
        state_col = "State", 
        value_col = "PercDemocratWasted",
        text_color = "gray", 
        font_size = 10, 
        state_border_col = "white",
        legend_position = "bottom"
      ) + 
        scale_fill_continuous(
          label = comma,
          low = "white", high = party_colors[1],
          limits = c(0, 100)
        ) + 
        guides(
          fill = guide_colorbar(
            title = "Percent \'Wasted\' \nDemocratic Votes", barwidth = 20,
            label.theme = element_text(size = 24),
            title.theme = element_text(size = 24))
        ) + 
        theme(legend.position = "bottom")
    }
    if(input$toPlot == "WaV" & input$party == "Republican"){
      myPlot = statebins_continuous(
        elections_state_year %>% dplyr::filter(Year == input$election), 
        state_col = "State", 
        value_col = "PercRepublicanWasted",
        text_color = "gray", 
        font_size = 10, 
        state_border_col = "white",
        legend_position = "bottom"
      ) + 
        scale_fill_continuous(
          label = comma,
          low = "white", high = party_colors[2],
          limits = c(0, 100)
        ) + 
        guides(
          fill = guide_colorbar(
            title = "Percent \'Wasted\' \nRepublican Votes", barwidth = 20,
            label.theme = element_text(size = 24),
            title.theme = element_text(size = 24))
        ) + 
        theme(legend.position = "bottom")
    }
    if(input$toPlot == "WaV" & input$party == "All"){
      myPlot = statebins_continuous(
        elections_state_year %>% dplyr::filter(Year == input$election), 
        state_col = "State", 
        value_col = "PercWasted",
        text_color = "gray", 
        font_size = 10, 
        state_border_col = "white",
        legend_position = "bottom"
      ) + 
        scale_fill_continuous(
          label = comma,
          low = "white", high = "purple",
          limits = c(0, 100)
        ) + 
        guides(
          fill = guide_colorbar(
            title = "Percent \'Wasted\' \nVotes", barwidth = 20,
            label.theme = element_text(size = 24),
            title.theme = element_text(size = 24))
        ) + 
        theme(legend.position = "bottom")
    }
    if(input$toPlot == "WaV" & input$party == "Independent"){
      myPlot = statebins_continuous(
        elections_state_year %>% dplyr::filter(Year == input$election), 
        state_col = "State", 
        value_col = "PercOtherWasted",
        text_color = "gray", 
        font_size = 10, 
        state_border_col = "white",
        legend_position = "bottom"
      ) + 
        scale_fill_continuous(
          label = comma,
          low = "white", high = party_colors[3],
          limits = c(0, 100)
        ) + 
        guides(
          fill = guide_colorbar(
            title = "Percent \'Wasted\' \nIndependent Votes", barwidth = 20,
            label.theme = element_text(size = 24),
            title.theme = element_text(size = 24))
        ) + 
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
  
  output$state_info = renderUI({
    nearestState = nearPoints(statebins_coords, input$plot_click, xvar = "x", yvar = "y", threshold = 40)[1,1]
    stateInfo = elections_state_year %>%
      filter(State == nearestState,
             Year == input$election)
    if(!is.na(nearestState)){
      # Change this
      HTML(
        paste(
          "<font size = \"+2\">",
          nearestState, "<br/>",
          "Total Votes:", as.character(stateInfo$total), "<br/>",
          "% Democratic Votes:", as.character(round(stateInfo$DVotes/stateInfo$Total * 100, 2)), "<br/>",
          "% Democratic Seats:", as.character(round(stateInfo$D/stateInfo$Reps * 100, 2)), "<br/>",
          "% \'Wasted\' Votes:", as.character(round(stateInfo$PercWasted, 2)), "<br/>",
          "Ratio (D Votes:Seats):", as.character(round(stateInfo$DVotes / stateInfo$Total / stateInfo$D * stateInfo$Reps, 4)), "<br/>",
          "</font> <br/>",
          "<font size = \"-1\"> A ratio of 1 means the distribution of votes perfectly matches the distribution of seats. <br/>
          A ratio < 1 means the proportion of Democratic seats is greater than the proportion of Democrat votes. <br/>
          A ratio > 1 means the proportion of Republican seats is greater than the proportion of Republican votes. <br/> </font>"
        )
      )
    }
  })
  
}

shinyApp(ui, server)