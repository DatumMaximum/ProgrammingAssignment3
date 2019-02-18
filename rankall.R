## This function returns the a list of hospitals across states 
## based on the rank number and outcome provided by the user.
  


rankall <- function(outcome, num = "best") {

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
            df0 <- rd[,c('State', 'Hospital.Name', outcome)]
            colnames(df0) <- c('state', 'hospital', 'days')
            df1 <- df0[complete.cases(df0[,c('state', 'hospital', 'days')]),]

            # splits the data based on state
            df2 <- split(df1, df1$state)
            df3 <- c()
            
            
            # loops through the split data, orders and ranks each set. 
            i <- 1
            end <- length(df2)
            while (i <= end) {
                
                dat <- df2[[i]]
                h <- dat[order(dat[c('days','hospital')], na.last = NA),]
                h$rank <- NA
                h$rank[order(h$days, h$hospital)] <- 1:nrow(h)
                df3[[i]] <- h[complete.cases(h),]
                
                i <- i + 1
            
            }
            

            # prints each hospital in each state based on rank.
            for (n in df3) {
                
                n <- n[complete.cases(n),]
                print(n[n$rank[num], c('hospital', 'state')])
                
            }
            
}