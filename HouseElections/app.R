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
load("./Data/data.rda")
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
                    # HTML("Click on a state to see the results of its districts' elections.</br> (If no results or zeroes are shown, the data may be missing.)"),
                    width = 12,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    div(style = "overflow-x: scroll", dataTableOutput("click_info"))))),
      tabItem(tabName = "about",
              fluidRow(
                box(h2("About"), align = "center", width = 6, height = 85),
                box(imageOutput("polisLogo2"), align = "center", width = 6, height = 85)),
              fluidRow(
                box(width = 12, title = HTML("<h2><center>What am I looking at?</h2></center>"),
                    HTML(read_file("./TextAssets/About.txt")))),
              fluidRow(
                box(width = 12, title = HTML("<h2><center>Credits</h2></center>"),
                    HTML(read_file("./TextAssets/Credits.txt"))))))))

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
        HTML("</br><center>1. Choose which year's</br>election to view, or hit \"play\"</br>to see a time-lapse.</center>"),
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
        HTML("</br><center>2. Choose a party to see</br>detailed stats, or \"Election Summary\"</br> for a state-by-state summary.</center>"),
        radioButtons(
          # Buttons to select party.
          inputId = "party",
          label = "Major Party:",
          choices = c("Election Summary", "Democratic", "Republican"),
          selected = "Election Summary"),
        HTML("</br><center>3. Choose which data to plot.
             </br>See \"About\" Tab for details.</center>"),
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
  
  output$map = renderPlot(execOnResize = FALSE, {
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
          geom_rect(data = rectangles2, mapping = aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax = ymax, fill = Party)) +
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
            this_year_state$R / (this_year_state$R + this_year_state$D + this_year_state$O) * as.numeric(rectangles2$Party[i] != "Republican") +
              this_year_state$D / (this_year_state$R + this_year_state$D + this_year_state$O) * as.numeric(rectangles2$Party[i] == "Other"))})
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
      ui_out = read_file("./TextAssets/ElectionSummaryOutput.txt")
      if(!is.na(nearestState)){
        # Filters data
        stateInfo = elections_state_year %>% filter(State == as.character(nearestState), Year == input$election)
        output$ratio_time = renderPlot(execOnResize = FALSE, {
          if(input$summaryPlot == "R"){
            d = elections_state_year %>% filter(State == nearestState) %>% drop_na(Dratio, Rratio, Oratio)
            a = ggplot(data = d) +
              theme(panel.background = element_blank()) + 
              geom_path(aes(x = Year, y = Dratio), color = party_colors[1]) +
              geom_path(aes(x = Year, y = Rratio), color = party_colors[2]) +
              geom_point(aes(x = Year, y = Dratio), color = party_colors[1], size = 2) +
              geom_point(aes(x = Year, y = Rratio), color = party_colors[2], size = 2) +
              geom_hline(aes(yintercept = 1), alpha=0.5) +
              ggtitle(paste(nearestState, "Representation Ratio by Party")) +
              ylab("Representation Ratio")
            if(any(d$Oratio != 0)){
              a = a +
                geom_path(aes(x = Year, y = Oratio), color = party_colors[3]) +
                geom_point(aes(x = Year, y = Oratio), color = party_colors[3], size = 2)
            }
          }
          if(input$summaryPlot == "S"){
            d = elections_state_year %>% filter(State == nearestState) %>% drop_na(D, R, O)
            a = ggplot(data = d) +
              theme(panel.background = element_blank()) +
              geom_path(aes(x = Year, y = D), color = party_colors[1]) +
              geom_path(aes(x = Year, y = R), color = party_colors[2]) +
              geom_point(aes(x = Year, y = D), color = party_colors[1], size = 2) +
              geom_point(aes(x = Year, y = R), color = party_colors[2], size = 2) +
              ggtitle(paste(nearestState, "House Representation by Party")) +
              ylab("Number of Representatives")
            if(any(d$O > 0)){
              a = a +
                geom_path(aes(x = Year, y = O), color = party_colors[3]) +
                geom_point(aes(x = Year, y = O), color = party_colors[3], size = 2)
            }
          }
          if(input$summaryPlot == "V"){
            d = elections_state_year %>% filter(State == nearestState) %>% drop_na(Dperc, Rperc, Operc)
            a = ggplot(data = d) +
              theme(panel.background = element_blank()) +
              geom_path(aes(x = Year, y = Dperc), color = party_colors[1]) +
              geom_path(aes(x = Year, y = Rperc), color = party_colors[2]) +
              geom_point(aes(x = Year, y = Dperc), color = party_colors[1], size = 2) +
              geom_point(aes(x = Year, y = Rperc), color = party_colors[2], size = 2) +
              ggtitle(paste("Vote Share by Party in", nearestState)) +
              ylab("Percentage of Votes")
            if(any(d$Operc > 15)){
              a = a + 
                geom_path(aes(x = Year, y = Operc), color = party_colors[3]) +
                geom_point(aes(x = Year, y = Operc), color = party_colors[3], size = 2)
            }
          }
          a})
        list(HTML(ui_out),plotOutput("ratio_time", width = 400, height = 200))}
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
             HTML(read_file("./TextAssets/StatePlotOutput.txt")))
      } else {
        # If a state isn't selected display this tooltip.
        HTML("<h3>Choose a state and party to see how district competitiveness stacks up over time.</h3>")}}})}
# Runs app
shinyApp(ui, server)