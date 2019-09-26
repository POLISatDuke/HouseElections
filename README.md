# HouseElections
POLIS project mapping US house election results

Before running this application make sure to have the following R packages installed:
-- shiny
-- readxl
-- tidyverse
-- shinydashboard
-- readr
-- scales
-- statebins
-- plotly

To update the application with new data, replace the "House Elections.xlsx" file with an updated one of the same format. Then run DataScript.R. It will populate "problems.csv" with any anomalous rows along with explanations of why those data were flagged, and save everything the app needs to run into a R data file. (If there are no issues detected with the data, "problems.csv" will have only a header row.)

Shiny apps can be hosted for free on shinyapps.io or with an open-source Shiny server. See https://shiny.rstudio.com/deploy/ for more details.