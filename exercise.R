best <- function(state, outcome){
        file <- read.csv("outcome-of-care-measures.csv", colClasses = "character")        
        file[, 11] <- as.numeric(file[, 11])
        names(file)[11] <- "Heart.Attack_Mortality"
        subfile <- subset(file, State == state & !is.na(Heart.Attack_Mortality),  select = c(outcome,"Heart.Attack_Mortality"))
        subfile <- subfile[sort(subfile$Heart.Attack_Mortality),]
        head(subfile)
}

best("AL", "City")