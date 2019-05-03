library(shiny)
library(readxl)
library(tidyverse)
library(shinydashboard)
library(readr)
library(fiftystater)
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
# Hex color codes for Dem Blue and Rep Red
party_colors <- c("#2E74C0", "#CB454A", "#F0E060")

# US state boundaries
data("fifty_states")

### Data load
# Removes parsing failures (mistakes in data formatting)
elections   = read_csv("./Data/house.csv")

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
        Other > Republican      & Republican > Democrat  ~ Other - Republican
      ),
    LosingVotes = 
      Republican   * as.numeric(Winner != "R") + 
      Democrat     * as.numeric(Winner != "D") + 
      Other        * as.numeric(Winner != "O"),
    LosingP = 
      LosingVotes / Total,
    WinningVotes =
      Republican   * as.numeric(Winner == "R") +
      Democrat     * as.numeric(Winner == "D") +
      Other        * as.numeric(Winner == "O"),
    WinningP = 
      WinningVotes / Total,
    ExcessVotes = 
      WinningVotes - SecondPlaceVotes,
    ExcessP = 
      WinningP / Total,
    WastedVotes = 
      ExcessVotes + LosingVotes,
    WastedP = 
      WastedVotes / Total,
    RepublicanWasted = 
      Republican                      * as.numeric(Winner != "R") + 
      (Republican - SecondPlaceVotes) * as.numeric(Winner == "R"),
    RepublicanWastedP = 
      RepublicanWasted / Republican,
    DemocratWasted = 
      Democrat                      * as.numeric(Winner != "D") + 
      (Democrat - SecondPlaceVotes) * as.numeric(Winner == "D"),
    DemocratWastedP = 
      DemocratWasted / Democrat,
    DemocratLosing = 
      Democrat * as.numeric(Winner != "D"),
    RepublicanLosing = 
      Republican * as.numeric(Winner != "R"),
    DemocratWinning = 
      Democrat * as.numeric(Winner == "D"),
    RepublicanWinning = 
      Republican * as.numeric(Winner == "R"),
    DemocratExcess = 
      (Democrat - SecondPlaceVotes) * as.numeric(Winner == "D"),
    RepublicanExcess = 
      (Republican - SecondPlaceVotes) * as.numeric(Winner == "R"))
elections_state_year = elections %>%
  group_by(State, Year) %>%
  summarize(
    R                 = sum(Winner == "R"),
    D                 = sum(Winner == "D"),
    total             = as.numeric(sum(Total)),
    DemSurplus        = D - R,
    Reps              = D + R,
    RVotes            = sum(Republican),
    DVotes            = sum(Democrat),
    OVotes            = sum(Other),
    LosingVotes       = sum(LosingVotes)/total * 100,
    WinningVotes      = sum(WinningVotes)/total * 100,
    ExcessVotes       = sum(ExcessVotes)/total * 100,
    WastedVotes       = sum(WastedVotes),
    RepublicanWasted  = sum(RepublicanWasted),
    DemocratWasted    = sum(DemocratWasted),
    RepublicanLosing  = sum(RepublicanLosing),
    DemocratLosing    = sum(DemocratLosing),
    RepublicanWinning = sum(RepublicanWinning),
    DemocratWinning   = sum(DemocratWinning),
    DemocratExcess    = sum(DemocratExcess),
    RepublicanExcess  = sum(RepublicanExcess)) %>% 
  mutate(
    PercWasted             = WastedVotes / total * 100,
    PercRepublicanWasted   = RepublicanWasted / RVotes * 100,
    PercDemocratWasted     = DemocratWasted / DVotes * 100,
    PercRepublicanLosing   = RepublicanLosing / RVotes * 100,
    PercDemocratLosing     = DemocratLosing / DVotes * 100,
    PercRepublicanWinning  = RepublicanWinning / RVotes * 100,
    PercDemocratWinning    = DemocratWinning / DVotes * 100,
    PercDemocratExcess     = DemocratExcess / DVotes * 100,
    PercRepublicanExcess   = RepublicanExcess/ RVotes * 100)
# Binning by state and year
elections_state_year$caption = paste0(elections_state_year$R, "R-", elections_state_year$D, "D")
elections_state_year$id = tolower(elections_state_year$State)

elections = tibble(Year = 0,
                   State = "0",
                   District = 0,
                   Democrat = 0,
                   Republican = 0,
                   Other = 0,
                   Total = 0,
                   Winner = "0",
                   Dperc = 0,
                   Rperc = 0,
                   Operc = 0)
for(i in seq(1, ncol(House_Elections), 12)){
  this_year = House_Elections[,i:(i+10)]
  names(this_year) = names(elections)
  this_year = this_year %>%
    filter(!is.na(Year),
           !is.na(State),
           !is.na(District),
           !is.na(Democrat),
           !is.na(Republican),
           !is.na(Total),
           !is.na(Winner),
           !is.na(Dperc),
           !is.na(Rperc),
           !is.null(Year),
           !is.null(State),
           !is.null(District),
           !is.null(Democrat),
           !is.null(Republican),
           !is.null(Total),
           !is.null(Winner),
           !is.null(Dperc),
           !is.null(Rperc)) %>%
    mutate(Other = case_when(
      is.na(Other) | is.null(Other) ~ 0,
      TRUE ~ as.numeric(Other)
    )) %>%
    mutate(Operc = case_when(
      is.na(Operc) | is.null(Operc) ~ 0,
      TRUE ~ as.numeric(Operc)
    )) %>%
    mutate(Dperc = as.numeric(Dperc)) %>%
    mutate(Rperc = as.numeric(Rperc))
  elections = rbind(elections, this_year)
}
elections = elections[-1,]
write.csv(elections, "Data/house.csv", row.names = FALSE)

# Sanity Checking
uploadProblems = function(uploaded){
  uploaded %>%
    # Vote percentage inequality should be in same direction as vote count inequality
    filter((Rperc>Dperc) != (Republican > Democrat) |
             # Winner should be consistent with vote count/percentage inequality
             Rperc>Dperc & Winner == "D" |
             Republican>Democrat & Winner == "D" |
             Rperc<Dperc & Winner == "R" |
             Republican<Democrat & Winner == "R" |
             # Zeros should match
             (Rperc == 0) != (Republican == 0) | 
             (Dperc == 0) != (Democrat == 0) |
             # Percentages correct (within some margin)
             !(((Republican / Total)*.95 <= Rperc) & ((Republican/Total)*1.05 >= Rperc)) |
             !(((Democrat / Total)*.95 <= Dperc) & ((Democrat/Total)*1.05 >= Dperc)) | 
             # Total Votes = Rep + Dem + Other votes
             Total != Democrat + Republican + Other) %>%
    mutate(problem_type = case_when(
      (Rperc>Dperc) != (Republican > Democrat) ~ "Vote percent and Vote count show different winners",
      # Winner should be consistent with vote count/percentage inequality
      Rperc>Dperc & Winner == "D" |
        Republican>Democrat & Winner == "D" |
        Rperc<Dperc & Winner == "R" |
        Republican<Democrat & Winner == "R"  ~ "Winner not consistent with vote count/percent",
      # Zeros should match
      (Rperc == 0) != (Republican == 0) | 
        (Dperc == 0) != (Democrat == 0) ~ "Non-matching zeros",
      # Percentages correct (within some margin)
      !(((Republican / Total)*.95 <= Rperc) & ((Republican/Total)*1.05 >= Rperc)) |
        !(((Democrat / Total)*.95 <= Dperc) & ((Democrat/Total)*1.05 >= Dperc)) ~ "Vote percentages off by more than 5%",
      # Total Votes = Rep + Dem + Other votes
      Total != Democrat + Republican + Other ~ "Total does not equal sum of party votes"
    ))
}

# Statebin Coordinates for clicks
statebins_coords = data.frame(
  State = c(
    "Hawaii", "Alaska", 
    "California", "Oregon", "Washington",
    "Arizona", "Utah", "Nevada", "Idaho",
    
    "New Mexico", "Colorado", "Wyoming", "Montana",
    "Texas", "Oklahoma", "Kansas", "Nebraska", "South Dakota", "North Dakota",
    "Louisiana", "Arizona", "Montana", "Iowa", "Minnesota",
    
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
    sidebarMenuOutput("sidebar"),
    imageOutput("polisLogo")
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "dataupload",
        h2("Data Upload"),
        fluidRow(
          box(
            fileInput(
              "upload", 
              "Augment Data", 
              accept = ".xlxs",
              buttonLabel = "Browse...",
              placeholder = "No file uploaded - Use 2018 Data"
            ),
            uiOutput("confirmbox")
          )
        ),
        fluidRow(
          uiOutput("problemBox")
        ),
        fluidRow(
          box(
            width = 12,
            title = "Glimpse Data",
            div(style = "overflow: scroll", dataTableOutput("extraData"))
          )
        )
      ),
      
      tabItem(
        tabName = "viz",
        h2("U.S. House Elections"),
        fluidRow(
          box(
            plotOutput("map", height = 550, click = clickOpts(id = "plot_click")), 
            width = 7, height = 575),
          box(
            div(htmlOutput("state_info")),
            height = 575,
            width = 5)
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
        h2("About"),
        fluidRow(
          box(
            "asdf"
          )
        )
      )
    )
  )
)

server = function(input, output){
  
  output$polisLogo = renderImage({
    return(list(src = "./polis-logo.jpg", contentType = "image/jpg", alt = "Alignment", width = 230, height = 50))
  }, deleteFile = FALSE)
  
  output$extraData = renderDataTable({
    infile = input$upload
    if(is.null(infile)){
      return(NULL)
    }
    upload = read_excel(infile$datapath)
    parsed = tibble(
      Year = 0,
      State = "0",
      District = 0,
      Democrat = 0,
      Republican = 0,
      Other = 0,
      Total = 0,
      Winner = "0",
      Dperc = 0,
      Rperc = 0,
      Operc = 0)
    for(i in seq(1, ncol(upload), 12)){
      this_year = upload[,i:(i+10)]
      names(this_year) = names(parsed)
      this_year = this_year %>%
        filter(
          !is.na(Year),
          !is.na(State),
          !is.na(District),
          !is.na(Democrat),
          !is.na(Republican),
          !is.na(Total),
          !is.na(Winner),
          !is.na(Dperc),
          !is.na(Rperc),
          !is.null(Year),
          !is.null(State),
          !is.null(District),
          !is.null(Democrat),
          !is.null(Republican),
          !is.null(Total),
          !is.null(Winner),
          !is.null(Dperc),
          !is.null(Rperc)) %>%
        mutate(
          Other = case_when(
            is.na(Other) | is.null(Other) ~ 0,
            TRUE ~ as.numeric(Other)
          )) %>%
        mutate(
          Operc = case_when(
            is.na(Operc) | is.null(Operc) ~ 0,
            TRUE ~ as.numeric(Operc)
          )) %>%
        mutate(Dperc = as.numeric(Dperc)) %>%
        mutate(Rperc = as.numeric(Rperc))
      parsed = rbind(parsed, this_year)
    }
    parsed = parsed[-1,]
    output$problemBox = renderUI({
      p = uploadProblems(parsed)
      if(nrow(p) > 0){
        fluidRow(
          box(
            width = 12,
            title = "Problems Detected",
            renderDataTable({
              p
            },
            options = list(
              pageLength = 10,
              searching = FALSE
            ))
          )
        )
      } else {
        fluidRow(
          box(
            title = "No Problems Detected"
          )
        )
      }
      
      
    })
    parsed
  },     
  options = list(
    pageLength = 10,
    searching = FALSE
  ))
  
  output$confirmbox = NULL # TO DO
  
  output$sidebar = renderMenu(
    {
      sidebarMenu(
        id = "sidebarmenu",
        menuItem("Data Upload", tabName = "dataupload", icon = icon("file-upload")),
        
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
            label = "Party:",
            choices = c("Democrat",
                        "Republican",
                        "All"),
            selected = "All"),
          
          radioButtons(
            inputId = "toPlot",
            label = "Color by:",
            choices = c("Losing Votes" = "LV",
                        "Winning Votes" = "WiV",
                        "Excess Votes" = "EV",
                        "Wasted Votes" = "WaV"),
            selected = "LV")
        ),
        
        menuItem("About", tabName = "about", icon = icon("info-circle"))
      )
    }
  )
  
  output$map = renderPlot({
    ### Subset and manipulate data for this year
    this_year = elections_state_year %>%
      dplyr::filter(Year == input$election)
    this_year_sp = left_join(fifty_states, this_year)
    
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
            title = "Percent Votes for \nLosing Candidates", barwidth = 20,
            label.theme = element_text(size = 24),
            title.theme = element_text(size = 24))
        ) + 
        theme(legend.position = "bottom")
    }
    if(input$toPlot == "LV" & input$party == "Democrat"){
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
          low = "white", high = "purple",
          limits = c(0, 100)
        ) + 
        guides(
          fill = guide_colorbar(title = "Percent Democrat Votes for \nLosing Candidates", barwidth = 20,
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
          low = "white", high = "purple",
          limits = c(0, 100)
        ) + 
        guides(
          fill = guide_colorbar(
            title = "Percent Republican Votes for \nLosing Candidates", barwidth = 20,
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
          low = "white", high = party_colors[3],
          limits = c(0, 100)
        ) + 
        guides(
          fill = guide_colorbar(
            title = "Percent Votes for \nWinning Candidates", barwidth = 20,
            label.theme = element_text(size = 24),
            title.theme = element_text(size = 24))
        ) + 
        theme(legend.position = "bottom")
    }
    if(input$toPlot == "WiV" & input$party == "Democrat"){
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
          low = "white", high = party_colors[3],
          limits = c(0, 100)
        ) + 
        guides(fill = guide_colorbar(
          title = "Percent Democrat Votes for \nWinning Candidates", barwidth = 20,
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
          low = "white", high = party_colors[3],
          limits = c(0, 100)
        ) + 
        guides(
          fill = guide_colorbar(
            title = "Percent Republican Votes for \nWinning Candidates", barwidth = 20,
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
    if(input$toPlot == "EV" & input$party == "Democrat"){
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
          low = "white", high = "purple",
          limits = c(0, 100)
        ) + 
        guides(
          fill = guide_colorbar(
            title = "Percent of Excess \nDemocrat Votes", barwidth = 20,
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
          low = "white", high = "purple",
          limits = c(0, 100)
        ) + 
        guides(fill = guide_colorbar(
          title = "Percent of Excess \nRepublican Votes", barwidth = 20,
          label.theme = element_text(size = 24),
          title.theme = element_text(size = 24))
        ) + 
        theme(legend.position = "bottom")
    }
    if(input$toPlot == "WaV" & input$party == "Democrat"){
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
            title = "Percent \'Wasted\' \nDemocrat Votes", barwidth = 20,
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
          low = "white", high = party_colors[1],
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
            title = "Percent \'Wasted\' Votes", barwidth = 20,
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
      HTML(
        paste(
          "<font size = \"+2\">",
          nearestState, "<br/>",
          "Total Votes:", as.character(stateInfo$total), "<br/>",
          "% Democrat Votes:", as.character(round(stateInfo$DVotes/stateInfo$Total * 100, 2)), "<br/>",
          "% Democrat Seats:", as.character(round(stateInfo$D/stateInfo$Reps * 100, 2)), "<br/>",
          "% \'Wasted\' Votes:", as.character(round(stateInfo$PercWasted, 2)), "<br/>",
          "Ratio (D Votes:Seats):", as.character(round(stateInfo$DVotes / stateInfo$Total / stateInfo$D * stateInfo$Reps, 4)), "<br/>",
          "</font> <br/>",
          "<font size = \"-1\"> A ratio of 1 means the distribution of votes perfectly matches the distribution of seats. <br/>
          A ratio < 1 means the proportion of Democrat seats is greater than the proportion of Democrat votes. <br/>
          A ratio > 1 means the proportion of Republican seats is greater than the proportion of Republican votes. <br/> </font>"
        )
      )
    }
  })
  
}

shinyApp(ui, server)