best <- function(state, outcome) {
  
        rd <- read.csv("outcome-of-care-measures.csv", na.strings = 'Not Available', stringsAsFactors = FALSE)
        
        if (outcome == 'heart attack') {
                  outcome <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'
        }else if (outcome == 'heart failure') {
                  outcome <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
        }else if (outcome == 'pneumonia'){
                  outcome <- 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
        }
        
        df <- rd[c(rd$State == state), c('State', 'Hospital.Name', outcome)]
        colnames(df) <- c('State', 'Hospital', 'Days')
        df1 <- df[order(df[c('Days','Hospital')], na.last = NA),]
        df1[c(1:5),]
        
        
        
}