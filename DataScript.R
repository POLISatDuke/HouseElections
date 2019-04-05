## Reshapes election data to more R-friendly format, saves as csv

library(readxl)
library(dplyr)
House_Elections = read_excel("Data/House Elections.xlsx")
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
  print(this_year[1,1])
  print(warnings())
  elections = rbind(elections, this_year)
}
elections = elections[-1,]
write.csv(elections, "Data/house.csv", row.names = FALSE)

# Sanity Checking
problems = elections %>%
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

write.csv(problems, "Data/problems.csv", row.names = FALSE)