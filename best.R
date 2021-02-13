best <- function(state, outcome){
        
        ## Read outcome data
        file <- read.csv("outcome-of-care-measures.csv", colClasses = "character")        
        
        ## Check that state is valid
        outcome_matrix <- matrix(c("heart attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "heart failure", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "pneumonia", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"), nrow = 3, ncol = 2, byrow = TRUE, dimnames = list(c("0", "1", "2"), c("user.outcome", "real_outcome")))
        outcome_test <- sapply(outcome_matrix[,1], function(x) x == outcome)
        if (sum(outcome_test) == 0) stop("invalid outcome")
        real_outcome <- outcome_matrix[outcome_test,2]
        
        ## Check that outcome is valid
        state.abb <- c(state.abb, "DC")
        if (!(state %in% state.abb)) stop("invalid state")
        
        # cast outcome
        file[, real_outcome] <- as.numeric(file[, real_outcome])
        
        # filter by state and outcome not NA, selecting only Hospital name and outcome
        subfile <- subset(file, State == state & !is.na(file[real_outcome]),  select = c("Hospital.Name" ,real_outcome))
        
        # order by outcome and hospital name
        subfile <- subfile[order(subfile[real_outcome], subfile["Hospital.Name"]),]
        
        # get first row of Hospital Name
        subfile[1,1]
}
