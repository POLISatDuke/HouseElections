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
  elections = rbind(elections, this_year)
}
elections = elections[-1,]
write.csv(elections, "Data/house.csv", row.names = FALSE)
