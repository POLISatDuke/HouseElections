library(shiny)
library(readxl)
library(tidyverse)
library(shinydashboard)
library(readr)
library(scales)
library(statebins)
library(plotly)
options(scipen = 999) # Turns of scientific notation
# Hex color codes for Dem Blue and Rep Red, Plus a Gold Color for Independents/Etc.
party_colors <- c("Democratic" = "#007AFF", "Republican" = "#FF000A", "Other" = "#FFE300")
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
        TRUE ~ 0),
    LosingVotes = 
      Republican   * as.numeric(Winner != "R") + 
      Democrat     * as.numeric(Winner != "D") + 
      Other        * as.numeric(Winner != "I"),
    LosingPerc = (LosingVotes / Total) * 100,
    WinningVotes =
      Republican   * as.numeric(Winner == "R") +
      Democrat     * as.numeric(Winner == "D") +
      Other        * as.numeric(Winner == "I"),
    WinningPerc      = (WinningVotes / Total) * 100,
    ExcessVotes      = WinningVotes - SecondPlaceVotes,
    ExcessPerc       = WinningPerc - LosingPerc,
    WastedVotes      = ExcessVotes + LosingVotes,
    WastedPerc       = (WastedVotes / Total) * 100,
    RepublicanWasted = 
      Republican                      * as.numeric(Winner != "R") + 
      (Republican - SecondPlaceVotes) * as.numeric(Winner == "R"),
    RepublicanWastedPerc = RepublicanWasted / Republican * 100,
    DemocratWasted = 
      Democrat                      * as.numeric(Winner != "D") + 
      (Democrat - SecondPlaceVotes) * as.numeric(Winner == "D"),
    DemocratWastedPerc = (DemocratWasted / Democrat) * 100,
    OtherWasted = 
      Other                      * as.numeric(Winner != "I") + 
      (Other - SecondPlaceVotes) * as.numeric(Winner == "I"),
    OtherWastedPerc   = OtherWasted / Other * 100,
    DemocratLosing    = Democrat * as.numeric(Winner != "D"),
    RepublicanLosing  = Republican * as.numeric(Winner != "R"),
    OtherLosing       = Other * as.numeric(Winner != "I"),
    DemocratWinning   = Democrat * as.numeric(Winner == "D"),
    RepublicanWinning = Republican * as.numeric(Winner == "R"),
    OtherWinning      = Other * as.numeric(Winner == "I"),
    DemocratExcess    = (Democrat - SecondPlaceVotes) * as.numeric(Winner == "D"),
    RepublicanExcess  = (Republican - SecondPlaceVotes) * as.numeric(Winner == "R"),
    OtherExcess       = (Other - SecondPlaceVotes) * as.numeric(Winner == "I"))
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
    Rperc                 = RVotes / total * 100,
    Dperc                 = DVotes / total * 100,
    Operc                 = OVotes / total * 100,
    PercWasted            = WastedVotes / total * 100,
    PercRepublicanWasted  = RepublicanWasted / RVotes * 100,
    PercDemocratWasted    = DemocratWasted / DVotes * 100,
    PercOtherWasted       = OtherWasted / OVotes * 100,
    PercRepublicanLosing  = RepublicanLosing / RVotes * 100,
    PercDemocratLosing    = DemocratLosing / DVotes * 100,
    PercOtherLosing       = OtherLosing / OVotes * 100,
    PercRepublicanWinning = RepublicanWinning / RVotes * 100,
    PercDemocratWinning   = DemocratWinning / DVotes * 100,
    PercOtherWinning      = OtherWinning / OVotes * 100,
    PercDemocratExcess    = DemocratExcess / DVotes * 100,
    PercRepublicanExcess  = RepublicanExcess/ RVotes * 100,
    PercOtherExcess       = OtherExcess / OVotes * 100)
# Expand table so that each state-year pair is in there (add NAs if no elections) 
# (Missing rows will disapper in statebin)
elections_state_year = elections_state_year %>% complete(State, Year = full_seq(Year, 2))
elections_state_year$caption = paste0(elections_state_year$R, "R-", elections_state_year$D, "D")
elections_state_year$id = tolower(elections_state_year$State)
# State-level calculations of fraction of votes by party, fraction of seats by party, and the ratio of the two
elections_state_year$Dfrac = elections_state_year$Dperc / (elections_state_year$Dperc + elections_state_year$Operc + elections_state_year$Rperc)
elections_state_year$Ofrac = elections_state_year$Operc / (elections_state_year$Dperc + elections_state_year$Operc + elections_state_year$Rperc)
elections_state_year$Rfrac = elections_state_year$Rperc / (elections_state_year$Dperc + elections_state_year$Operc + elections_state_year$Rperc)
elections_state_year$Dratio = (elections_state_year$D / (elections_state_year$D + elections_state_year$R + elections_state_year$O)) / elections_state_year$Dfrac
elections_state_year$Rratio = (elections_state_year$R / (elections_state_year$D + elections_state_year$R + elections_state_year$O)) / elections_state_year$Rfrac
elections_state_year$Oratio = (elections_state_year$O / (elections_state_year$D + elections_state_year$R + elections_state_year$O)) / elections_state_year$Ofrac
elections_state_year$Dratio[is.nan(elections_state_year$Dratio)|is.na(elections_state_year$Dratio)] = 0
elections_state_year$Rratio[is.nan(elections_state_year$Rratio)|is.na(elections_state_year$Rratio)] = 0
elections_state_year$Oratio[is.nan(elections_state_year$Oratio)|is.na(elections_state_year$Oratio)] = 0
# Find which ratio is biggest (which party is most overrepresented)
elections_state_year$maxRatio = sapply(1:nrow(elections_state_year), function(i){
  max(elections_state_year$Dratio[i], elections_state_year$Rratio[i], elections_state_year$Oratio[i])})
elections_state_year$maxParty = sapply(1:nrow(elections_state_year), function(i){
  c(c("Democratic", "Republican", "Other")[which(c(elections_state_year$Dratio[i], elections_state_year$Rratio[i], elections_state_year$Oratio[i]) == elections_state_year$maxRatio[i])])[1]})
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
    rep(1, 2), rep(2, 3), rep(3, 4),
    rep(4, 4), rep(5, 6), rep(6, 5),
    rep(7, 6), rep(8, 5), rep(9, 4),
    rep(10, 5), rep(11, 4), rep(12, 3)),
  y = c(
    8, 7, 5:3, 6:3,
    6:3, 8:3, 7:3,
    7:2, 7:3, 7:4,
    8, 6:3, 5:2, 
    4, 2, 1))
# Data frame for drawing rectangles over statebin plot
rectangles = rbind(statebins_coords %>% mutate(Party = "Republican"),
                   statebins_coords %>% mutate(Party = "Democratic"),
                   statebins_coords %>% mutate(Party = "Other")) %>% 
  mutate(xmin = x-.42, xmax = x+.42, ymin = y-.42, ymax = y+.42,
         abbr = rep(c("HI", "AK", "CA", "OR", "WA", "AZ", "UT", "NV", "ID",
                      "NM", "CO", "WY", "MT", "TX", "OK", "KA", "NE", "SD", "ND",
                      "LA", "AS", "MO", "IA", "MN", "MS", "TN", "KY", "IN", "IL", "WI",
                      "AL", "NC", "WV", "OH", "MI", "GA", "SC" , "VA", "PA",
                      "FL", "DC", "MD", "NJ", "NY", "DL", "CT", "MA", "VT", "RI", "NH", "ME"),3))
# Set up App User Interface
ui = dashboardPage(
  dashboardHeader(title = "U.S. House Elections"),
  dashboardSidebar(sidebarMenuOutput("sidebar")),
  dashboardBody(
    tabItems(
      tabItem(tabName = "viz",
              fluidRow(# Top Row: Title and Logo
                box(h2("U.S. House Elections"),
                    align = "center",
                    width = 7, height = 85),
                box(imageOutput("polisLogo"),
                    align = "center",
                    width = 5, height = 85)),
              fluidRow(# Second Row: Plot Outputs
                box(plotOutput("map", height = 550, click = clickOpts(id = "plot_click")), 
                    width = 7, height = 575),
                box(div(uiOutput("state_info")),
                    width = 5, height = 575)),
              fluidRow(# Third Row: Data Table Output
                box(title = "District Results",
                    HTML("Click on a state to see the results of its districts' elections.</br> (If no results or zeroes are shown, the data may be missing.)"),
                    width = 12,
                    height = 575,
                    collapsible = TRUE,
                    div(style = "overflow-x: scroll", dataTableOutput("click_info"))))),
      tabItem(tabName = "about",
              fluidRow(
                box(h2("About"), align = "center", width = 6, height = 85),
                box(imageOutput("polisLogo2"), align = "center", width = 6, height = 85)),
              fluidRow(
                box(width = 6, title = HTML("<h2><center>What am I looking at?</h2></center>"),
                    HTML("This visualization shows the results of U.S. House of Representatives elections. 
                  The statebin map on the left side of the page shows the results in a particular year, 
                  which you can selected using the slider input in the sidebar.
                  In the election summary view, you can compare how well a state's elected officials actually match the
                  votes cast on election day (in terms of party membership) using the 'Representation Ratio'. 
                  This is a limited measure, since smaller states with only one or a small number of 
                  districts will naturally have higher ratios and not all vote count data are available. But there are larger states
                  that consistently over-represent particular parties, which could indicate gerrymandering.
                  </br>If you select one of the major parties (step 2), the states will colored according to the criterion you choose:
                  </br>'Winning Votes' shows the percentage of votes cast for candidates of the chosen party that won their election.
                  </br>'Losing Votes' is the opposite; it shows what fraction of a party's votes went to losing candidates.
                  </br>'Excess Votes' is the difference between the winner's vote share and the runner-up's vote share.
                  It is smaller in close elections and larger in landslides.
                  </br> 'Wasted Votes' is the percentage of votes that were cast for a losing candidate or for a winning candidate in excess of the runner-up.
                  In principle, these people could have stayed home on election day and the result of the election would not have changed. (Losing + Excess = Wasted).
                  </br><center><h4>'Wasted Votes'?</h4></center>
                  We show 'Wasted' votes here as a crude measure of how well-represented a state's electorate is, but the name 'wasted' is imprecise and should not be taken too seriously.
                  If a population somehow cooperated to reduce the number of wasted votes (without changing their political preferences), 
                  the equilibrium result would be just one person going to vote for the most popular candidate on election day and everyone else staying home.
                  This is technically a dictatorship, and certainly not how we should want elections to go.")),
                box(width = 6, title = HTML("<h2><center>Credits</h2></center>")))))))

plottingChoices = c("Vote Share" = "V", "Winning Votes" = "WiV", "Losing Votes" = "LV", "Excess Votes" = "EV", "Wasted Votes" = "WaV")

server = function(input, output, session){
  
  # Load images for logos (need one for each page)
  output$polisLogo = renderImage({
    return(list(src = "./polis-logo.jpg", contentType = "image/jpg", 
                alt = "Alignment", width = 300, height = 60))}, deleteFile = FALSE)
  
  output$polisLogo2 = renderImage({
    return(list(src = "./polis-logo.jpg", contentType = "image/jpg", 
                alt = "Alignment", width = 300, height = 60))}, deleteFile = FALSE)
  
  # Sidebar menu is an output object so it may react to user inputs.
  output$sidebar = renderMenu({
    sidebarMenu(
      id = "sidebarmenu",
      menuItem("About", tabName = "about", icon = icon("info-circle")), # About section
      menuItem("Representation by State", tabName = "viz", icon = icon("chart-area"), selected = TRUE),
      conditionalPanel(
        "input.sidebarmenu == 'viz'", # Displays only if 'viz' panel is being viewed. 
        # This panel shows options for mapping.
        HTML("</br><center>1. Choose which year's</br>election to view,or hit 'play'</br>to see a time-lapse.</center>"),
        sliderInput(
          # Slider to select year.
          inputId = "election",
          label = "Election Year ",
          min = min(elections_state_year$Year),
          max = max(elections_state_year$Year),
          value = max(elections_state_year$Year),
          step = 2,
          animate = TRUE,
          width = 300,
          ticks = FALSE,
          sep = ""),
        HTML("</br><center>2. Choose a party to see</br>detailed stats, or 'All' for a</br>state-by-state summary.</center>"),
        radioButtons(
          # Buttons to select party.
          inputId = "party",
          label = "Major Party:",
          choices = c("Election Summary", "Democratic", "Republican"),
          selected = "Election Summary"),
        HTML("</br><center>3. Choose which data to plot.
             </br>See 'About' Tab for details.</center>"),
        conditionalPanel(
          "input.party != 'Election Summary'",
          radioButtons(
            # Buttons to select what data to plot.
            inputId = "toPlot",
            label = "Show me:",
            choices = plottingChoices,
            selected = "V")),
        conditionalPanel(
          "input.party == 'Election Summary'",
          radioButtons(
            inputId = "summaryPlot",
            label = "Show me:",
            choices = c("Votes by Party" = "V",
                        "House Seats by Party" = "S",
                        "Representation Ratio" = "R"),
            selected = "R"),
          HTML("<center>The Representation Ratio compares</br>the number of Representatives each</br>party elected to the proportion</br>of votes that they won.</center>"))))})
  
  output$map = renderPlot({
    # Below Generates plot per plotting options
    if(input$party == "Election Summary"){
      # What follows is my janky imitation of a statebins plot
      this_year = elections_state_year %>% filter(Year == input$election)
      if(input$summaryPlot == "V"){
        # Plot Order is R, D, and then O, so only need to adjust starting points for latter 2 (others will overwrite)
        rectangles2 = rectangles
        rectangles2$xmin = sapply(1:nrow(rectangles2), function(i){
          this_year_state = this_year %>% filter(State == rectangles2$State[i])
          rectangles2$xmin[i] + 0.84 * ( 
            this_year_state$Rfrac * as.numeric(rectangles2$Party[i] != "Republican") +
              this_year_state$Dfrac * as.numeric(rectangles2$Party[i] == "Other"))})
        myPlot = ggplot() +
          geom_rect(data = rectangles2, 
                    mapping = aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax = ymax, fill = Party)) +
          theme_void() +
          ggtitle("Voting Distributions by State") +
          theme(plot.title = element_text(size = 25, hjust = 0.5, vjust = -5)) +
          guides(fill = guide_legend(title = NULL, label.theme = element_text(size = 24), nrow = 3,
                                     label.position = "right")) + 
          xlim(0.5, 12.5) + ylim(10.3, 0.3) +
          theme(legend.position = c(0.5,.1)) +
          scale_fill_manual(values = party_colors) +
          geom_text(data = rectangles2, size = 9, aes(x = x, y = y, label = abbr))}
      if(input$summaryPlot == "S"){
        rectangles2 = plyr::join(rectangles, this_year)
        rectangles2$xmin = sapply(1:nrow(rectangles2), function(i){
          this_year_state = this_year %>% filter(State == rectangles2$State[i])
          rectangles2$xmin[i] + 0.84 * ( 
            this_year_state$Rfrac * as.numeric(rectangles2$Party[i] != "Republican") +
              this_year_state$Dfrac * as.numeric(rectangles2$Party[i] == "Other"))})
        myPlot = ggplot() +
          geom_rect(data = rectangles2, 
                    mapping = aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax = ymax, fill = Party)) +
          theme_void() +
          ggtitle("Representatives by Party") +
          theme(plot.title = element_text(size = 25, hjust = 0.5, vjust = -5)) +
          guides(fill = guide_legend(title = NULL, label.theme = element_text(size = 24), nrow = 3,
                                     label.position = "right")) + 
          xlim(0.5, 12.5) + ylim(10.3, 0.3) +
          theme(legend.position = c(0.5,.1)) +
          scale_fill_manual(values = party_colors) +
          geom_text(data = rectangles2, size = 7, aes(x = x, y = y-.2, label = abbr)) + 
          geom_text(data = rectangles2, size = 4, aes(x = x, y = y+.2, label = caption, fontface = "bold"))
      }
      if(input$summaryPlot == "R"){
        rectangles2 = plyr::join(rectangles, this_year) %>% filter(Party == maxParty)
        myPlot = ggplot() +
          geom_rect(data = rectangles2, 
                    mapping = aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax = ymax, fill = Party, alpha = maxRatio)) +
          theme_void() +
          ggtitle("Over-Representation in the\nU.S. House of Representatives") +
          theme(plot.title = element_text(size = 25, hjust = 0.5, vjust = -5)) +
          guides(fill = guide_legend(title = "Most Overrepresented Party", 
                                     title.theme = element_text(size = 20), 
                                     label.theme = element_text(size = 20), nrow = 3,
                                     label.position = "right"),
                 alpha = "none") +
          xlim(0.5, 12.5) + ylim(10.3, 0.3) +
          theme(legend.position = c(0.5,.1)) +
          scale_fill_manual(values = party_colors) +
          geom_text(data = rectangles2, size = 7, aes(x = x, y = y-.2, label = abbr)) + 
          geom_text(data = rectangles2, size = 4, aes(x = x, y = y+.2, label = round(maxRatio,2)))
      }
      myPlot} else {
        if(input$toPlot == "LV" & input$party == "Democratic"){
          myPlot = statebins_continuous(
            elections_state_year %>% dplyr::filter(Year == input$election), 
            state_col = "State", 
            value_col = "PercDemocratLosing",
            text_color = "black" , 
            font_size = 9, 
            state_border_col = "white",
            legend_position = "bottom") + 
            scale_fill_continuous(
              label = comma,
              low = "white", high = party_colors[1],
              limits = c(0, 100)) + 
            guides(
              fill = guide_colorbar(title = "Percent Democratic Votes \nfor Losing Candidates", barwidth = 20,
                                    label.theme = element_text(size = 24),
                                    title.theme = element_text(size = 24))) + 
            theme(legend.position = "bottom")}
        if(input$toPlot == "LV" & input$party == "Republican"){
          myPlot = statebins_continuous(
            elections_state_year %>% dplyr::filter(Year == input$election), 
            state_col = "State", 
            value_col = "PercRepublicanLosing",
            text_color , 
            font_size = 9, 
            state_border_col = "white",
            legend_position = "bottom")+ 
            scale_fill_continuous(
              label = comma,
              low = "white", high = party_colors[2],
              limits = c(0, 100)) + 
            guides(
              fill = guide_colorbar(
                title = "Percent Republican Votes \nfor Losing Candidates", barwidth = 20,
                label.theme = element_text(size = 24),
                title.theme = element_text(size = 24))) + 
            theme(legend.position = "bottom")}
        if(input$toPlot == "WiV" & input$party == "Democratic"){
          myPlot = statebins_continuous(
            elections_state_year %>% dplyr::filter(Year == input$election), 
            state_col = "State", 
            value_col = "PercDemocratWinning",
            text_color = "black" , 
            font_size = 9, 
            state_border_col = "white",
            legend_position = "bottom") + 
            scale_fill_continuous(
              label = comma,
              low = "white", high = party_colors[1],
              limits = c(0, 100)) + 
            guides(fill = guide_colorbar(
              title = "Percent Democratic Votes \nfor Winning Candidates", barwidth = 20,
              label.theme = element_text(size = 24),
              title.theme = element_text(size = 24))) + 
            theme(legend.position = "bottom")}
        if(input$toPlot == "WiV" & input$party == "Republican"){
          myPlot = statebins_continuous(
            elections_state_year %>% dplyr::filter(Year == input$election), 
            state_col = "State", 
            value_col = "PercRepublicanWinning",
            text_color = "black" , 
            font_size = 9, 
            state_border_col = "white",
            legend_position = "bottom") + 
            scale_fill_continuous(
              label = comma,
              low = "white", high = party_colors[2],
              limits = c(0, 100)) + 
            guides(
              fill = guide_colorbar(
                title = "Percent Republican Votes \nfor Winning Candidates", barwidth = 20,
                label.theme = element_text(size = 24),
                title.theme = element_text(size = 24))) + 
            theme(legend.position = "bottom")}
        if(input$toPlot == "EV" & input$party == "Democratic"){
          myPlot = statebins_continuous(
            elections_state_year %>% dplyr::filter(Year == input$election), 
            state_col = "State", 
            value_col = "PercDemocratExcess",
            text_color = "black" , 
            font_size = 9, 
            state_border_col = "white",
            legend_position = "bottom") + 
            scale_fill_continuous(
              label = comma,
              low = "white", high = party_colors[1],
              limits = c(0, 100)) + 
            guides(
              fill = guide_colorbar(
                title = "Percent of Excess \nDemocratic Votes", barwidth = 20,
                label.theme = element_text(size = 24),
                title.theme = element_text(size = 24))) + 
            theme(legend.position = "bottom")}
        if(input$toPlot == "EV" & input$party == "Republican"){
          myPlot = statebins_continuous(
            elections_state_year %>% dplyr::filter(Year == input$election), 
            state_col = "State", 
            value_col = "PercRepublicanExcess",
            text_color = "black" , 
            font_size = 9, 
            state_border_col = "white",
            legend_position = "bottom") + 
            scale_fill_continuous(
              label = comma,
              low = "white", high = party_colors[2]) + 
            guides(fill = guide_colorbar(
              title = "Percent of Excess \nRepublican Votes", barwidth = 20,
              label.theme = element_text(size = 24),
              title.theme = element_text(size = 24))) + 
            theme(legend.position = "bottom")}
        if(input$toPlot == "WaV" & input$party == "Democratic"){
          myPlot = statebins_continuous(
            elections_state_year %>% dplyr::filter(Year == input$election), 
            state_col = "State", 
            value_col = "PercDemocratWasted",
            text_color = "black" , 
            font_size = 9, 
            state_border_col = "white",
            legend_position = "bottom") + 
            scale_fill_continuous(
              label = comma,
              low = "white", high = party_colors[1]) + 
            guides(
              fill = guide_colorbar(
                title = "Percent \'Wasted\' \nDemocratic Votes", barwidth = 20,
                label.theme = element_text(size = 24),
                title.theme = element_text(size = 24))) + 
            theme(legend.position = "bottom")}
        if(input$toPlot == "WaV" & input$party == "Republican"){
          myPlot = statebins_continuous(
            elections_state_year %>% dplyr::filter(Year == input$election), 
            state_col = "State", 
            value_col = "PercRepublicanWasted",
            text_color = "black" , 
            font_size = 9, 
            state_border_col = "white",
            legend_position = "bottom") + 
            scale_fill_continuous(
              label = comma,
              low = "white", high = party_colors[2]) + 
            guides(
              fill = guide_colorbar(
                title = "Percent \'Wasted\' \nRepublican Votes", barwidth = 20,
                label.theme = element_text(size = 24),
                title.theme = element_text(size = 24))) + 
            theme(legend.position = "bottom")}
        if(input$toPlot == "V" & input$party == "Democratic"){
          myPlot = statebins_continuous(
            elections_state_year %>% dplyr::filter(Year == input$election), 
            state_col = "State", 
            value_col = "Dperc",
            text_color = "black" , 
            font_size = 9, 
            state_border_col = "white",
            legend_position = "bottom") + 
            scale_fill_continuous(
              label = comma,
              low = "white", high = party_colors[1]) + 
            guides(
              fill = guide_colorbar(
                title = "Percent Democratic Votes", barwidth = 20,
                label.theme = element_text(size = 24),
                title.theme = element_text(size = 24))) + 
            theme(legend.position = "bottom")}
        if(input$toPlot == "V" & input$party == "Republican"){
          myPlot = statebins_continuous(
            elections_state_year %>% dplyr::filter(Year == input$election), 
            state_col = "State", 
            value_col = "Rperc",
            text_color = "black" , 
            font_size = 9, 
            state_border_col = "white",
            legend_position = "bottom") + 
            scale_fill_continuous(
              label = comma,
              low = "white", high = party_colors[2]) + 
            guides(
              fill = guide_colorbar(
                title = "Percent Republican Votes", barwidth = 20,
                label.theme = element_text(size = 24),
                title.theme = element_text(size = 24))) + 
            theme(legend.position = "bottom")}
        myPlot}})
  # Below renders data table output for clicked state and selected year
  output$click_info = renderDataTable({
    # Finds state that was clicked
    nearestState = nearPoints(statebins_coords, input$plot_click, xvar = "x", yvar = "y", threshold = 40)[1,1]
    # Filters, Transforms, and Formats Data 
    elections %>% 
      filter(State == nearestState,
             Year == input$election) %>%
      mutate("% Dem." = percent(Dperc), "% Rep." = percent(Rperc), "% Other" = percent(Operc)) %>%
      select(Year, State, District, Winner, Democrat, Republican, Other, Total, "% Dem.", "% Rep.", "% Other") %>%
      rename("Democratic" = Democrat)},     
    options = list(
      # Display options for the table
      pageLength = 10,
      searching = FALSE))
  # Below renders district competitiveness graph for clicked state
  output$state_info = renderUI({
    # Find clicked state
    nearestState = NA
    if(!is.null(input$plot_click)){
      nearestState = nearPoints(statebins_coords, input$plot_click, xvar = "x", yvar = "y", threshold = 40)[1,1]}
    if(input$party == "Election Summary"){
      ui_out = "</br><center><h3>Visualizing U.S. House of Representatives Elections</h3></center>
      </br>The 'Representation Ratio' is calculated by dividing the proportion of elected representatives from
      a party by the proportion of votes won by that party. For example, a Repubiclan ratio of 2 means that,
      by proportion, there are twice as many Republican representatives as Republican votes. States are colored
      by which party is most overrepresented and saturated according to the scale of over-representation.
      </br>If you choose 'Votes by Party' or 'House Seats by Party', each state will be colored in 
      proportion to the fraction of votes or representatives belonging to a particular party. Click on a state to see
      a time series graph."
      if(!is.na(nearestState)){
        # Filters data
        stateInfo = elections_state_year %>% filter(State == as.character(nearestState), Year == input$election)
        # There could probably be a neat graph added here (ratio vs time. colored by party?)
        output$ratio_time = renderPlot({
          if(input$summaryPlot == "R"){
            a = ggplot(data = elections_state_year %>% filter(State == nearestState) %>% drop_na(Dratio, Rratio, Oratio)) +
              theme(panel.background = element_blank()) + 
              geom_path(aes(x = Year, y = Dratio), color = party_colors[1]) +
              geom_path(aes(x = Year, y = Rratio), color = party_colors[2]) +
              geom_path(aes(x = Year, y = Oratio), color = party_colors[3]) +
              geom_point(aes(x = Year, y = Dratio), color = party_colors[1], size = 2) +
              geom_point(aes(x = Year, y = Rratio), color = party_colors[2], size = 2) +
              geom_point(aes(x = Year, y = Oratio), color = party_colors[3], size = 2) +
              geom_hline(aes(yintercept = 1), alpha=0.5) +
              ggtitle(paste(nearestState, "Representation Ratio by Party")) +
              ylab("Representation Ratio")
          }
          if(input$summaryPlot == "S"){
            a = ggplot(data = elections_state_year %>% filter(State == nearestState) %>% drop_na(D, R, O)) +
              theme(panel.background = element_blank()) +
              geom_path(aes(x = Year, y = D), color = party_colors[1]) +
              geom_path(aes(x = Year, y = R), color = party_colors[2]) +
              geom_path(aes(x = Year, y = O), color = party_colors[3]) +
              geom_point(aes(x = Year, y = D), color = party_colors[1], size = 2) +
              geom_point(aes(x = Year, y = R), color = party_colors[2], size = 2) +
              geom_point(aes(x = Year, y = O), color = party_colors[3], size = 2) +
              ggtitle(paste(nearestState, "House Representation by Party")) +
              ylab("Number of Representatives")
          }
          if(input$summaryPlot == "V"){
            a = ggplot(data = elections_state_year %>% filter(State == nearestState) %>% drop_na(Dperc, Rperc, Operc)) +
              theme(panel.background = element_blank()) +
              geom_path(aes(x = Year, y = Dperc), color = party_colors[1]) +
              geom_path(aes(x = Year, y = Rperc), color = party_colors[2]) +
              geom_path(aes(x = Year, y = Operc), color = party_colors[3]) +
              geom_point(aes(x = Year, y = Dperc), color = party_colors[1], size = 2) +
              geom_point(aes(x = Year, y = Rperc), color = party_colors[2], size = 2) +
              geom_point(aes(x = Year, y = Operc), color = party_colors[3], size = 2) +
              ggtitle(paste("Vote Share by Party in", nearestState)) +
              ylab("Percentage of Votes")
          }
          a})
        list(HTML(ui_out),plotOutput("ratio_time", width = "auto", height = 250))}
      else{
        HTML(ui_out)
      }
    } else {
      if(!is.na(nearestState)){
        # Filters data
        stateInfo = elections %>% filter(State == as.character(nearestState))
        stateInfo = rbind(
          stateInfo %>% group_by(Year) %>% 
            summarise(Party = "Democratic", VoteShare = sum(Democrat) / (sum(Democrat+Republican+Other)),
                      SeatShare = sum(Winner == "D") / n()),
          stateInfo %>% group_by(Year) %>% 
            summarise(Party = "Republican", VoteShare = sum(Republican) / (sum(Democrat+Republican+Other)),
                      SeatShare = sum(Winner == "R") / n()),
          stateInfo %>% group_by(Year) %>% 
            summarise(Party = "Other", VoteShare = sum(Other) / (sum(Democrat+Republican+Other)),
                      SeatShare = sum(Winner == "O") / n())) %>% 
          filter(Party == input$party)
        output$statePlot = renderPlotly({
          p = ggplot() + 
            geom_point(data = stateInfo, aes(x = VoteShare, y = SeatShare, frame = Year),
                       color = party_colors[input$party], size = 5) +
            geom_text(data = stateInfo %>% filter(Year %% 5 == 0 | Year == min(Year) | Year == max(Year)), 
                      alpha = 0.5, aes(x = VoteShare, y = SeatShare, label = Year)) +
            geom_point(data = stateInfo, size = 2, alpha = 0.25, color = party_colors[input$party],
                       aes(x = VoteShare, y = SeatShare, text = paste0(Year, 
                                                                       "<br>Votes: ", 100*round(VoteShare,3), "%",
                                                                       "<br>Reps: ", 100*round(SeatShare, 3),"%"))) +
            geom_abline(intercept = 0, slope = 1) +
            ggtitle(paste(as.character(nearestState), "Elections")) + xlab("Vote Fraction") + ylab("Representatives Fraction")
          
          p = ggplotly(p, tooltip = c("text")) %>%
            animation_opts(1000, easing = "cubic-in-out")
          p})
        list(plotlyOutput("statePlot"),
             HTML("The black line represents perfect representation: the fraction of voters for a particular party 
                  is exactly the fraction of the state's representatives that are from that party. Parties above the line are over-represented;
                  they have a larger share of the state's representatives than the state's votes.
                  <br>Hit play to watch the state's representation change over time. Hover over points to view precise values."))
      } else {
        # If a state isn't selected display this tooltip.
        HTML("<h3>Choose a state and party to see how district competitiveness stacks up over time.</h3>")}}})}
# Runs app
shinyApp(ui, server)