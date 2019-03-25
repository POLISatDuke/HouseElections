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
###

### Various handy objects
# Hex color codes for Dem Blue and Rep Red
party_colors <- c("#2E74C0", "#CB454A")
# Hexagon Radius (center to vertex)
radius = 1
offset = sqrt(radius^2 + radius^2/4)

# US state boundaries
data("fifty_states")
# Calculate centroids by state
states = unique(fifty_states$id)
states = states[states!="district of columbia"]
centroids = data.frame(state = states, long = rep(0,50), lat = rep(0,50))
for(state in states){
  centroids[centroids$state == state, 2:3] = geosphere::centroid(fifty_states[fifty_states$id == state, c("long", "lat")])
}

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
  dplyr::mutate(demSurplus = D - R,
                reps = D + R)
# Binning to state-years
elections_state_year$caption = paste0(elections_state_year$R, "R-", elections_state_year$D, "D")
elections_state_year$id = tolower(elections_state_year$State)
# Calculating gradient limits
maxD = max(elections_state_year$D)
maxR = max(elections_state_year$R)
maxTot = max(elections_state_year$total)
maxD_R = max(elections_state_year$demSurplus)
minD_R = min(elections_state_year$demSurplus)
maxReps = max(elections_state_year$reps)
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
                 choices = c("States", "Statebins", "Seats"),
                 selected = "Statebins"),
    
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
          guides(fill = guide_colorbar(title = "Democrat Seats", barwidth = 20)) + 
          theme(legend.position = "bottom",
                legend.text = element_text(size = 24),
                legend.title = element_text(size = 24))
      }
      
      if(input$toPlot == "R"){
        myPlot = myPlot + aes(fill = R) +
          scale_fill_continuous(low = "#ffffff", 
                                high = party_colors[2], 
                                limits = c(0, maxR)) + 
          guides(fill = guide_colorbar(title = "Republican Seats", barwidth = 20)) + 
          theme(legend.position = "bottom",
                legend.text = element_text(size = 24),
                legend.title = element_text(size = 24))
      }
      
      if(input$toPlot == "D-R"){
        myPlot = myPlot + aes(fill = demSurplus) +
          scale_fill_gradient2(low = party_colors[2], 
                               high = party_colors[1]) + 
          guides(fill = guide_colorbar(title = "Democrat Seats - Republican Seats", barwidth = 20)) +
          theme(legend.position = "bottom",
                legend.text = element_text(size = 24),
                legend.title = element_text(size = 24))
      }
      
      if(input$toPlot == "D+R"){
        myPlot = myPlot + aes(fill = reps) + 
          scale_fill_continuous(low = "white", high = "purple",
                                limits = c(NA, maxReps)) + 
          guides(fill = guide_colorbar(title = "Total Representation", barwidth = 20)) +
          theme(legend.position = "bottom",
                legend.text = element_text(size=24),
                legend.title = element_text(size=24))
      }
      
      if(input$toPlot == "V"){
        myPlot = myPlot + aes(fill = total) + 
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
      
    }
    if(input$display == "Statebins"){
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
                                      value_col = "demSurplus",
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
                                      value_col = "reps",
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
                                      value_col = "total",
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
    }
    if(input$display == "Seats"){
      ### Step 1: Assign hexes from earlier to districts and states
      elections_year = elections[elections$Year == input$election,]
      elections_year$long = NA
      elections_year$lat = NA
      for(state in states){
        elections_year[tolower(elections_year$State)==state,c("long", "lat")] = centroids[centroids$state==state, c("long", "lat")]
      }
      set.seed(1) # Want to jitter hexes in reproducible manner
      elections_year$long = elections_year$long + rnorm(length(elections_year$long), mean = 0, sd = 0.1)
      elections_year$lat = elections_year$lat + rnorm(length(elections_year$lat), mean = 0, sd = 0.1)
      
      elections_year = elections_year %>% 
        dplyr::mutate(plotOrder = rank(-lat - long))
      elections_year = elections_year[order(elections_year$plotOrder),]
      stateOrder = elections_year %>%
        group_by(State) %>%
        summarise(ord = max(plotOrder))
      stateOrder = stateOrder$State[order(stateOrder$ord)]
      elections_year = elections_year[order(elections_year$plotOrder),]
      elections_year$hexlong = NA
      elections_year$hexlat = NA
      availableHexes = round(elections_year[1,c("lat","long")],0)
      usedHexes = data.frame(lat = NULL, long = NULL)
      for(state in stateOrder){
        for(i in 1:nrow(elections_year)){
          if(elections_year$State[i] == state){
            pt = elections_year[i, c("lat", "long")]
            availableHexes = availableHexes %>% 
              dplyr::mutate(distance = (lat - unlist(pt[1,1]))^2 + (long - unlist(pt[1,2]))^2)
            myHex = availableHexes[availableHexes$distance == min(availableHexes$distance),]
            usedHexes = rbind(usedHexes, myHex[1,1:2])
            elections_year[i, c("hexlong", "hexlat")] = myHex[1,1:2]
            newAvailable = data.frame(lat = rep(unlist(myHex[1,1]),18) + c(-2, 0, 2, -3, -1, 1, 3, -4, -2, 2, 4, -3, -1, 1, 3, -2, 0, 2),
                                      long = rep(unlist(myHex[1,2]),18) + c(4, 4, 4, 2, 2, 2, 2, 0, 0, 0, 0, -2, -2, -2, -2, -4, -4, -4))
            availableHexes = unique(rbind(availableHexes[,1:2], newAvailable))
            availableHexes = dplyr::anti_join(availableHexes, usedHexes)
          } 
        }
      }
      elections_year$hexlong = elections_year$hexlong - 
        10 * as.numeric(elections_year$State == "Hawaii") - 
        20 * as.numeric(elections_year$State == "Alaska")
      
      # From midpoints calculate vertices to draw entire hexes
      elections_year_hex1 = elections_year %>% 
        mutate(hexlat = hexlat + sqrt(2/3),
               hexlong = hexlong + 1)
      elections_year_hex2 = elections_year %>% 
        mutate(hexlat = hexlat + 4/3)
      elections_year_hex3 = elections_year %>%
        mutate(hexlat = hexlat + sqrt(2/3),
               hexlong = hexlong - 1)
      elections_year_hex4 = elections_year %>%
        mutate(hexlat = hexlat - sqrt(2/3),
               hexlong = hexlong - 1)
      elections_year_hex5 = elections_year %>%
        mutate(hexlat = hexlat - 4/3)
      elections_year_hex6 = elections_year %>%
        mutate(hexlat = hexlat - sqrt(2/3),
               hexlong = hexlong + 1)
      elections_year_hex = rbind(elections_year_hex1, elections_year_hex2, elections_year_hex3, 
                                 elections_year_hex4, elections_year_hex5, elections_year_hex6) %>%
        mutate(id = paste0(State, District))
      state_borders = elections_year_hex %>% select(State, hexlong, hexlat)
      myPlot = ggplot(data = elections_year_hex) + 
        geom_polygon(mapping = aes(x = hexlong,
                                   y = hexlat,
                                   id = id,
                                   fill = Winner)) + 
        theme_void()
      
    }
    myPlot
  })
  
  output$hover_info = renderPrint({
    cat("input$plot_hover:\n")
    str(input$plot_hover)
  })
  
}

shinyApp(ui, server)