rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state, outcome, and rank are valid
    a = state %in% data$State
    if (a == FALSE){
        stop("Invalid State")
    }
    
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    b = outcome %in% outcomes
    if (b == FALSE){
        stop("Invalid Outcome")
    } 
    
    numvals <- c("best", "worst", 1:length(data$State))
    c = num %in% numvals
    if (c == FALSE){
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
    suppressWarnings(sdata <- data.frame(data$State, data$Hospital.Name, as.numeric(outcomeData))) #must use as.numeric on `outcomeData` to prevent transferring this into class `factor`
    x <- subset(sdata, sdata$data.State == state)           # Gather rows with the desired state
    x <- subset(x, x$as.numeric.outcomeData. != "NA")       # Remove incomplete data
    x <- x[order(x$data.Hospital.Name),]                    # Reorder data to deal with ties
    x <- x[order(x$as.numeric.outcomeData.),]               # Reorder data to deal with ties
    
    ## rate
    # Return hospital with desired rank 
    # Dealing with "best"
    if (num == "best"){
        num = 1
    }
    # Dealing with "worst"
    if (num == "worst"){
        result <- c(as.list(x[length(x[,2]),2]))
    } else {
        result <- c(as.list(x[as.numeric(num),2]))
    }
    answer <- as.character(unlist(result))
    return(answer)
}