
library(dplyr)
library(caret)
library(lubridate)

#### Please note that the odds are collected: 
# - Odds for weekend Fridays afternoons not later than 17:00 British Standard Time. 
# - Odds for midweek fixtures are collected Tuesdays not later than 15:00 British Standard Time.

start <- Sys.time()

# DOWNLOAD CURRENT ODDS -----------------------------------------------------------
scoring <- tbl_df(read.csv("http://www.football-data.co.uk/fixtures.csv",header = T,stringsAsFactors = F))
scoring <- scoring %>% filter(Div == "I1")

#drop NA's filled columns
scoring <- scoring[,colSums(is.na(scoring))<nrow(scoring)]
#keep only complete cases
scoring <- scoring %>% filter(complete.cases(scoring))



# SELECT AVAILABLE TRAINING DATA ------------------------------------------------
load("./input/full_2015-03-07.rda")

#keep only target var and columns available in scoring
full <- tbl_df(full[,c("FTR",colnames(scoring))])

#keep only complete cases
full <- full %>% filter(complete.cases(full))

#format target var
full <- full %>% mutate(FTR = factor(FTR))
full$FTR <- relevel(full$FTR,ref = "Draw")
full$FTR <- relevel(full$FTR,ref = "Home")



# SPLIT TRAIN-TEST MODEL -------------------------------------------------------------
ix <- createDataPartition(full$FTR,p = .8,list = F)
training <- tbl_df(full[ix,])
test <- tbl_df(full[-ix,])

#drop non-predictor columns
training <- training %>% select(-(Div:AwayTeam))
test <- test %>% select(-(Div:AwayTeam))



# TRAIN MODEL -------------------------------------------------------------
grid <- expand.grid(mtry = c(15,18,20,22,25))
ctrl <- trainControl(method = "repeatedcv",number = 5,repeats = 3,verboseIter = T,classProbs = T)

set.seed(1234)
rf <- train(data = training,FTR~.,method = "rf",tuneGrid = grid,trControl = ctrl)

#check results
rf
plot(rf)

#assess on test
testresults <- predict(rf,newdata = test)
confusionMatrix(testresults,test$FTR)



# SCORE -------------------------------------------------------------------
predictions <- scoring %>% select(Div:AwayTeam)
predictions <- tbl_df(cbind(predictions, Predicted = predict(rf,newdata = scoring),predict(rf,newdata = scoring,type = "prob")))

#drop "Draws" for poor predictive power
predictions <- predictions %>% filter(Predicted != "Draw")

Sys.time()-start


t(scoring)

