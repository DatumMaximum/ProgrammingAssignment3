
## Finds a hospital bast on state, outcome and rank provided by user. 

rankhospital <- function(state, outcome, num = 'best') {
        
        ## reads csv in
        rd <- read.csv("outcome-of-care-measures.csv", na.strings = 'Not Available', stringsAsFactors = FALSE)
        
        # control structure that allows simplified input of health outcome
        if (outcome == 'heart attack') {
                outcome <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'
        }else if (outcome == 'heart failure') {
                outcome <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
        }else if (outcome == 'pneumonia'){
                outcome <- 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
        }
        
        
        # creates a data frame of all required variables and removes incomplete entries
        df <- rd[c(rd$State == state), c('State', 'Hospital.Name', outcome)]
        colnames(df) <- c('State', 'Hospital', 'Days')
        df1 <- df[order(df[c('Days','Hospital')], na.last = NA),]
        df2 <- df1[complete.cases(df1),]
        

        # controls for case where num = best or worst
        if (num == 'best') {
            num = 1
        }else if (num == 'worst'){
            num = length(df2$Days)
        }
        
        # prints output to user 
        df2$Rank <- NA
        df2$Rank[order(df2$Days, df2$Hospital)] <- 1:nrow(df2)
        x <- df2[df2$Rank[num], 'Hospital' ]
        #y <- df2[1:10,] #used for testing
        
        print(x)
        
        
}