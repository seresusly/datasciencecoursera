best <- function(state, outcome) {
    
    ## Read outcome data
    data <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    x = state %in% data$State
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    y = outcome %in% outcomes
    if (x == FALSE){
        stop("Invalid State")
    }
    if (y == FALSE){
        stop("Invalid Outcome")
    } 
    
    ## Return hospital name in that state with lowest 30-day death
    if (outcome == "heart attack"){
        outcomeData = data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
    }
    if (outcome == "heart failure"){
        outcomeData = data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
    }
    if (outcome == "pneumonia"){
        outcomeData = data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
    }
    sdata <- data.frame(data$State, data$Hospital.Name, as.numeric(outcomeData)) #must use as.numeric on `outcomeData` to prevent transferring this into class `factor`
    z <- subset(sdata, data$State == state)      # Gather rows with the desired state
    ## rate
    
    hname <- c()
    for (i in 1:length(z)){
        zz <- z[i] == min((z[,3]), na.rm = TRUE) #compare the outcome data to the minimum
    }
    result <- data.frame(z[,2], zz)
    fresult <- subset(result, result[,2] == TRUE)
    afresult <- fresult[order(fresult$z...2.),] #Alphabetize results data frame
    print(afresult[1,1])
}