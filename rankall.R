rankall <- function(outcome, num = "best") {
            
            rd <- read.csv("outcome-of-care-measures.csv", na.strings = 'Not Available', stringsAsFactors = FALSE)
    
            if (outcome == 'heart attack') {
                    outcome <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'
            }else if (outcome == 'heart failure') {
                    outcome <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
            }else if (outcome == 'pneumonia'){
                    outcome <- 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
            }
    
            
            df0 <- rd[,c('State', 'Hospital.Name', outcome)]
            colnames(df0) <- c('state', 'hospital', 'days')
            df1 <- df0[complete.cases(df0[,c('state', 'hospital', 'days')]),]
            df2 <- split(df1, df1$state)
            df3 <- c()
            
            
            
            i <- 1
            end <- length(df2)
            while (i <= end) {
                
                dat <- df2[[i]]
                h <- dat[order(dat[c('days','hospital')], na.last = NA),]
                h$rank <- NA
                h$rank[order(h$days, h$hospital)] <- 1:nrow(h)
                df3[[i]] <- h[complete.cases(h),]
                
                i <- i + 1
                #print(head(h))
            
            }
            
            for (n in df3) {
                
                n <- n[complete.cases(n),]
                print(n[n$rank[num], c('hospital', 'state')])
                
            }
            
}