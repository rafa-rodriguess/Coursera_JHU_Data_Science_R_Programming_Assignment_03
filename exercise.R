rankall <- function(outcome, num){

        ## Read outcome data
        file <- read.csv("outcome-of-care-measures.csv", colClasses = "character")        
        
        ## Check that state is valid
        outcome_matrix <- matrix(c("heart attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "heart failure", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "pneumonia", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"), nrow = 3, ncol = 2, byrow = TRUE, dimnames = list(c("0", "1", "2"), c("user.outcome", "real_outcome")))
        outcome_test <- sapply(outcome_matrix[,1], function(x) x == outcome)
        if (sum(outcome_test) == 0) stop("invalid outcome")
        real_outcome <- outcome_matrix[outcome_test,2]
        
        # cast outcome
        file[, real_outcome] <- as.numeric(file[, real_outcome])
        
        
        subRank <- function(file, state, real_outcome){
                # filter by state and outcome not NA, selecting only Hospital name and outcome
                subfile <- subset(file, State == state & !is.na(file[real_outcome]),  select = c("Hospital.Name" ,"State", real_outcome))
        
                # order by outcome and hospital name
                subfile <- subfile[order(subfile[real_outcome], subfile["Hospital.Name"]),]
                
                # get first row of Hospital Name
                if (!is.numeric(num)){ 
                        if (!(num %in% c("best","worst"))) stop("invalid ranking position (num argument)")
                        if (num == "best")   num <- 1
                        if (num == "worst")  num <- nrow(subfile)
                }
                
                if (nrow(subfile) < num) return(c(NA, state))
                subfile[num,1:2]
        }
        
        ## For each state, find the hospital of the given rank
        resp <- data.frame()
        state.abb <- c(state.abb, "DC")

        for (state in state.abb){
                resp <- rbind(resp, subRank(file, state, real_outcome))
        }
        resp <- resp[order(resp["State"]),]
        names(resp)[1] <- "hospital"
        names(resp)[2] <- "state"
        resp
}

