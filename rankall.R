rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
    states <- unique(data$State)
    states <- states[order(states)]                   # Order the subsetted dataframe best to worst based on desired outcome
    
    ## Check that outcome, and rank are valid
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    a = outcome %in% outcomes
    if (a == FALSE){
        stop("Invalid Outcome")
    } 
    
    numvals <- c("best", "worst", 1:length(data[,1]))
    b = num %in% numvals
    if (b == FALSE){
        stop("Invalid Rank")
    }
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    if (outcome == "heart attack"){
        outcomeData = data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
    }
    if (outcome == "heart failure"){
        outcomeData = data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
    }
    if (outcome == "pneumonia"){
        outcomeData = data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
    }
    suppressWarnings(sdata <- data.frame(data$State, data$Hospital.Name, as.numeric(outcomeData))) #must use as.numeric on `outcomeData` to prevent transferiing this into class `factor`
    y <- sdata[order(sdata$as.numeric.outcomeData),]           # Order the subsetted dataframe best to worst based on desired outcome
    y <- subset(y, y$as.numeric.outcomeData. != "NA")          # Remove incomplete data
    g <- split(y$as.numeric.outcomeData, y$data.State)         # Split the data into outcomes for each state
    
    ## For each state, find the hospital of the given rank
    # Dealing with "best"
    if (num == "best"){
        num = 1
    }
    
    # Dealing with "worst"
    if (num == "worst"){
        #get the rank value for each state
        x <- c()
        result <- c()
        for (i in 1:length(g)){
            x <- subset(y, y$data.State == states[i])
            x <- x[order(x$data.Hospital.Name),]         # Reorder data to deal with ties
            x <- x[order(x$as.numeric.outcomeData.),]    # Reorder data to deal with ties
            result[i] <- c(as.list(x[length(x[,2]),2]))
        }
    } else {
        #get the rank value for each state
        x <- c()
        result <- c()
        for (i in 1:length(g)){
            x <- subset(y, y$data.State == states[i])
            x <- x[order(x$data.Hospital.Name),]         # Reorder data to deal with ties
            x <- x[order(x$as.numeric.outcomeData.),]    # Reorder data to deal with ties
            result[i] <- c(as.list(x[as.numeric(num),2]))
        }
        
    }
    answer <- data.frame(unlist(result), states)
    colnames(answer) <- c("hospital", "state")
    return(answer)
}