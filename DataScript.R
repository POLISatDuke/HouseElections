## Reshapes election data to more R-friendly format, saves as csv

library(readxl)
library(dplyr)
library(tidyverse)
House_Elections = read_excel("./HouseElections/Data/House Elections.xlsx")
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
#  print(this_year[1,1])
#  print(warnings())
  elections = rbind(elections, this_year)
}
elections = elections[-1,]
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
elections_state_year$maxParty = factor(elections_state_year$maxParty, levels = c("Democratic", "Republican", "Other"))
# Sanity Checking
problems = elections %>%
  mutate(problem_type = case_when(
    (Rperc > Operc) & (Rperc > Dperc) & (Winner != "R") |
      (Dperc > Operc) & (Dperc > Rperc) & (Winner != "D") |
      (Operc > Dperc) & (Operc > Rperc) & (Winner != "I") ~ "Winner is not consistent with vote percent",
    (Republican > Democrat) & (Republican > Other) & (Winner != "R") |
      (Democrat > Other) & (Democrat > Republican) & (Winner != "D") |
      (Other > Democrat) & (Other > Republican) & (Winner != "I") ~ "Winner is not consistent with vote count",
    # Percentages correct (within some margin)
    !(((Republican / Total)*.95 <= Rperc) & ((Republican/Total)*1.05 >= Rperc)) |
      !(((Democrat / Total)*.95 <= Dperc) & ((Democrat/Total)*1.05 >= Dperc)) |
      !(((Other / Total)*.95 <= Operc) & ((Other / Total)*1.05 >= Operc)) ~ "Vote percentages off by more than 5%",
    # Total Votes = Rep + Dem + Other votes
    Total != Democrat + Republican + Other ~ "Total does not equal sum of party votes",
    TRUE ~ "NA"
  )) %>%
  filter(problem_type != "NA")

write.csv(problems, "./HouseElections/Data/problems.csv", row.names = FALSE)
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
  x = c(rep(1, 2), rep(2, 3), rep(3, 4),
        rep(4, 4), rep(5, 6), rep(6, 5),
        rep(7, 6), rep(8, 5), rep(9, 4),
        rep(10, 5), rep(11, 4), rep(12, 3)),
  y = c(8, 7, 5:3, 6:3,
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
rectangles$Party = factor(rectangles$Party, levels = c("Democratic", "Republican", "Other"))

save(elections, elections_state_year, statebins_coords, rectangles, file = "./HouseElections/Data/data.rda")