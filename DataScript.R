## Reshapes election data to more R-friendly format, saves as csv

library(readxl)
library(dplyr)
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
write.csv(elections, "./HouseElections/Data/house.csv", row.names = FALSE)

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