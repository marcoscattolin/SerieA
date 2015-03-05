setwd("~/GitHub/SerieA")
library(dplyr)
library(caret)

#### Please note that the odds are collected: 
# - Odds for weekend Fridays afternoons not later than 17:00 British Standard Time. 
# - Odds for midweek fixtures are collected Tuesdays not later than 15:00 British Standard Time.

# DOWNLOAD CURRENT ODDS -----------------------------------------------------------
scoring <- tbl_df(read.csv("http://www.football-data.co.uk/fixtures.csv",header = T,stringsAsFactors = F))



# SELECT AVAILABLE COLUMNS ------------------------------------------------
load("full_2015-03-05.rda")
training <- full[,colnames(scoring)]
