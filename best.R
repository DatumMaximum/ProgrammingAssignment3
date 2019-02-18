## This script reads in data from the outcome-of-care-measures.csv file and 
## returns the best hospital for a particular outcome and state. 
## I have configured the script to return the top 5 hospitals for a particular state.

best <- function(state, outcome) {
        
        # reads in the csv fle
        rd <- read.csv("outcome-of-care-measures.csv", na.strings = 'Not Available', stringsAsFactors = FALSE)
        
        # control structure that allows simplified input of health outcome
        if (outcome == 'heart attack') {
                  outcome <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'
        }else if (outcome == 'heart failure') {
                  outcome <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
        }else if (outcome == 'pneumonia'){
                  outcome <- 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
        }
        
        # sorts the data based on state, hospital name and outcome and returns a short list to the user with results.
        df <- rd[c(rd$State == state), c('State', 'Hospital.Name', outcome)]
        colnames(df) <- c('State', 'Hospital', 'Days')
        df1 <- df[order(df[c('Days','Hospital')], na.last = NA),]
        df1[c(1:5),]
        
        
        
}