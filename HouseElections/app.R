library(shiny)
library(readxl)
library(tidyverse)
library(shinydashboard)
library(readr)
library(scales)
library(statebins)
options(scipen = 999) # Turns of scientific notation
# Hex color codes for Dem Blue and Rep Red, Plus a Gold Color for Independents/Etc.
party_colors <- c("Democratic" = "#007AFF", "Republican" = "#FF000A", "Independent" = "#FFE300")
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
      Other        * as.numeric(Winner != "O"),
    LosingPerc = (LosingVotes / Total) * 100,
    WinningVotes =
      Republican   * as.numeric(Winner == "R") +
      Democrat     * as.numeric(Winner == "D") +
      Other        * as.numeric(Winner == "O"),
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
      Other                      * as.numeric(Winner != "O") + 
      (Other - SecondPlaceVotes) * as.numeric(Winner == "O"),
    OtherWastedPerc   = OtherWasted / Other * 100,
    DemocratLosing    = Democrat * as.numeric(Winner != "D"),
    RepublicanLosing  = Republican * as.numeric(Winner != "R"),
    OtherLosing       = Other * as.numeric(Winner != "O"),
    DemocratWinning   = Democrat * as.numeric(Winner == "D"),
    RepublicanWinning = Republican * as.numeric(Winner == "R"),
    OtherWinning      = Other * as.numeric(Winner == "O"),
    DemocratExcess    = (Democrat - SecondPlaceVotes) * as.numeric(Winner == "D"),
    RepublicanExcess  = (Republican - SecondPlaceVotes) * as.numeric(Winner == "R"),
    OtherExcess       = (Other - SecondPlaceVotes) * as.numeric(Winner == "O"))
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
election_summary_state_year = elections_state_year 
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
# Set up App User Interface
ui = dashboardPage(
  dashboardHeader(title = "U.S. House Elections"),
  dashboardSidebar(sidebarMenuOutput("sidebar")),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "viz",
        fluidRow(
          # Top Row: Title and Logo
          box(
            h2("U.S. House Elections"),
            align = "center",
            width = 7, height = 85),
          box(
            imageOutput("polisLogo"),
            align = "center",
            width = 5, height = 85)),
        fluidRow(
          # Second Row: Plot Outputs
          box(
            plotOutput("map", height = 550, click = clickOpts(id = "plot_click")), 
            width = 7, height = 575),
          box(
            div(uiOutput("state_info")),
            width = 5, height = 575)),
        fluidRow(
          # Third Row: Data Table Output
          box(
            title = "District Results",
            HTML("Click on a state to see the results of its districts' elections.</br>
                 (If no results are shown the data may be missing, but keep in mind that 
                 not all states have house elections every two years.)"),
            width = 12,
            height = 575,
            collapsible = TRUE,
            div(style = "overflow-x: scroll", dataTableOutput("click_info"))))),
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            h2("About"),
            align = "center",
            width = 6, height = 85),
          box(
            imageOutput("polisLogo2"),
            align = "center",
            width = 6, height = 85)),
        fluidRow(
          box(
            width = 6, title = HTML("<h2><center>What am I looking at?</h2></center>"),
            HTML("This visualization shows the results of U.S. House of Representatives elections. 
                  The statebin map on the left side of the page shows the results in a particular year, 
                  which you can selected using the slider input in the sidebar. 
                  The states are colored according the party and criterion you choose:
                  </br>'Winning Votes' shows the percentage of votes cast for candidates of the chosen party that won their election.
                  </br>'Losing Votes' is the opposite; it shows what fraction of a party's votes went to losing candidates.
                  </br>'Excess Votes' is the difference between the winner's vote share and the runner-up's vote share.
                  It is smaller in close elections and larger in landslides.
                  </br> 'Wasted Votes' is the percentage of votes that were cast for a losing candidate or for a winning candidate in excess of the runner-up.
                  In principle, these people could have stayed home on election day and the result of the election would not have changed. (Losing + Excess = Wasted).
                  </br><center><h3>'Wasted Votes'?</h3></center>
                  We show 'Wasted' votes here as a crude measure of how well-represented a state's electorate is, but the name 'wasted' is imprecise and should not be taken too seriously.
                  If a population somehow cooperated to reduce the number of wasted votes (without changing their political preferences), 
                  the equilibrium result would be just one person going to vote for the most popular candidate on election day and everyone else staying home.
                  This is technically a dictatorship, and certainly not how we should want elections to go.")),
          box(
            width = 6, title = HTML("<h2><center>Credits</h2></center>")))))))

plottingChoices = c("Vote Share" = "V",
                    "Winning Votes" = "WiV",
                    "Losing Votes" = "LV",
                    "Excess Votes" = "EV",
                    "Wasted Votes" = "WaV")

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
          choices = c("Democratic", "Republican", "Election Summary"),
          selected = "Democratic"),
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
                        "Representation Ratio" = "R")),
          HTML("<center>The Representation Ratio compares</br>the number of Representatives each</br>party elected to the proportion</br>of votes that they won.</center>")
        )))})
  
  output$map = renderPlot({
    # Subset and manipulate data for this year
    this_year = elections_state_year %>%
      dplyr::filter(Year == input$election)
    # Notes on color choices: All plots use a single color gradient. 
    # If a major party is selected, gradient is white to their color.
    # All is white to Purple, and Other is white to gold.
    # Below Generates plot per plotting options
    if(input$party == "Election Summary"){
      myPlot = statebins_continuous(
        elections_state_year %>% dplyr::filter(Year == input$election), 
        state_col = "State",
        value_col = NULL,
        text_color = "gray", 
        font_size = 10, 
        state_border_col = "white",
        legend_position = "bottom")}
    if(input$toPlot == "LV" & input$party == "Democratic"){
      myPlot = statebins_continuous(
        elections_state_year %>% dplyr::filter(Year == input$election), 
        state_col = "State", 
        value_col = "PercDemocratLosing",
        text_color = "gray", 
        font_size = 10, 
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
        text_color = "gray", 
        font_size = 10, 
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
        text_color = "gray", 
        font_size = 10, 
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
        text_color = "gray", 
        font_size = 10, 
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
        text_color = "gray", 
        font_size = 10, 
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
        text_color = "gray", 
        font_size = 10, 
        state_border_col = "white",
        legend_position = "bottom") + 
        scale_fill_continuous(
          label = comma,
          low = "white", high = party_colors[2],
          limits = c(0, 100)) + 
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
        text_color = "gray", 
        font_size = 10, 
        state_border_col = "white",
        legend_position = "bottom") + 
        scale_fill_continuous(
          label = comma,
          low = "white", high = party_colors[1],
          limits = c(0, 100)) + 
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
        text_color = "gray", 
        font_size = 10, 
        state_border_col = "white",
        legend_position = "bottom") + 
        scale_fill_continuous(
          label = comma,
          low = "white", high = party_colors[2],
          limits = c(0, 100)) + 
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
        text_color = "gray", 
        font_size = 10, 
        state_border_col = "white",
        legend_position = "bottom") + 
        scale_fill_continuous(
          label = comma,
          low = "white", high = party_colors[1],
          limits = c(0, 100)) + 
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
        text_color = "gray", 
        font_size = 10, 
        state_border_col = "white",
        legend_position = "bottom") + 
        scale_fill_continuous(
          label = comma,
          low = "white", high = party_colors[2],
          limits = c(0, 100)) + 
        guides(
          fill = guide_colorbar(
            title = "Percent Republican Votes", barwidth = 20,
            label.theme = element_text(size = 24),
            title.theme = element_text(size = 24))) + 
        theme(legend.position = "bottom")}
    myPlot})
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
    nearestState = nearPoints(statebins_coords, input$plot_click, xvar = "x", yvar = "y", threshold = 40)[1,1]
    if(!is.na(nearestState) & input$party != "Election Summary"){
      # Filters data
      stateInfo = elections %>% filter(State == nearestState)
      # Counts number of districts in the state per year
      nDistricts = stateInfo %>% group_by(Year) %>% summarise(n = n()) %>% select(n)
      # If a state has a lot of districts we don't want to display everything.
      nQuantiles = max(2, min(5, min(nDistricts))) # Calculates a reasonable number of ribbons to draw
      # Finds census years are in the time range
      censusYears = seq(from = 10 * ceiling(min(stateInfo$Year)/10),
                        to = 10 * floor(max(stateInfo$Year)/10), by = 10)
      # Render plot
      output$statePlot = renderPlot({
        stateInfo2 = switch(input$party,
                            Republican = stateInfo[,c("Year", "Rperc")],
                            Democratic = stateInfo[,c("Year", "Dperc")])
        stateInfo3 = data.frame(Year = NULL, Value = NULL, Quantile = NULL)
        quantiles = seq(0, 1, length.out = nQuantiles)
        years = unique(stateInfo2$Year)
        for(i in 1:length(years)){
          # This loop adds one row per quantile-year to stateInfo3; ribbons are drawn on each quantile
          values = stateInfo2 %>% filter(Year == years[i]) %>% select(-Year)
          thisYear = data.frame(
            Year = rep(years[i], nQuantiles),
            Value = unname(quantile(as.vector(t(values)), probs = quantiles)),
            Quantile = quantiles)
          stateInfo3 = rbind(stateInfo3, thisYear)}
        # Sets up plot
        statePlot = ggplot() +
          ggtitle(paste0(nearestState, ": Election Competitiveness Over Time")) +
          theme(panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank()) +
          xlab("Year")
        # It would make sense for dems to display blue charts and independents to display gold... 
        # I have no clue why this doesn't work - any party displays in Red despite choosing the colors below.
        selected_color = switch(input$party,
                                Democratic = party_colors[1],
                                Republican = party_colors[2])
        for(i in 1:(nQuantiles-1)){
          # Draws a ribbon between each quantile
          statePlot = statePlot + 
            geom_ribbon(
              aes_(
                x = stateInfo3$Year[stateInfo3$Quantile == 0], 
                ymin = stateInfo3$Value[stateInfo3$Quantile == quantiles[i]],
                ymax = stateInfo3$Value[stateInfo3$Quantile == quantiles[i+1]],
                fill = selected_color,
                color = selected_color,
                alpha = (i/nQuantiles)))}
        if(nQuantiles >= 3){
          # Adds a legend if there are more than 2 quantiles.
          statePlot = statePlot +           
            scale_alpha(breaks = 1:(nQuantiles-1)/nQuantiles,
                        labels = c(paste("Least", input$party),
                                   rep(" ", max(0,nQuantiles-3)),
                                   paste("Most", input$party)),
                        guide = guide_legend(
                          label.position = "bottom",
                          title = "Districts",
                          title.position = "top"))
        } else {
          # Otherwise there's no legend.
          statePlot = statePlot + guides(alpha = FALSE)}
        # Finishing touches on plot: axis lines on 50% and census years, 
        # axis limits that make sense, aesthetic choices
        statePlot +
          geom_vline(mapping = NULL, xintercept = censusYears, alpha = 0.25) +
          geom_hline(mapping = NULL, yintercept = 0.5, alpha = 0.5, color = "white") +
          xlim(min(stateInfo3$Year), max(stateInfo3$Year)) +
          ylim(0,1) +
          theme(legend.position = "bottom") +
          guides(fill = FALSE) +
          ylab("Vote Share") +
          guides(color = FALSE)})
      plotOutput("statePlot")
    } else {
      # If a state isn't selected display this tooltip.
      HTML("Choose a state and party to see how district competitiveness stacks up over time.")}})}
# Runs app
shinyApp(ui, server)