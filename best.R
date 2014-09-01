best <- function(state, outcome) {
    outcomeColumn <-
        if (outcome == "heart attack") "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        else if (outcome == "heart failure") "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        else if (outcome == "pneumonia") "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        else 0
    if (outcomeColumn == 0) stop("invalid outcome")
    else
    {
        outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        outcomeData <- outcomeData[, c("Hospital.Name", "State", outcomeColumn)]
        outcomeData <- subset(outcomeData, State == state)
        if (nrow(outcomeData) == 0) stop("invalid state")
        else
        {
            outcomeRatingColumnIndex <- 3
            outcomeDataMaxRowIndex <- which.min(outcomeData[, outcomeRatingColumnIndex])
            outcomeDataMaxRow <- outcomeData[outcomeDataMaxRowIndex, ]
            outcomeDataMaxRow$Hospital.Name
        }
    }
}

# Tests
test <- function () {
    print(best("TX", "heart attack"))
    print(best("TX", "heart failure"))
    print(best("MD", "heart attack"))
    print(best("MD", "pneumonia"))
    print(best("BB", "heart attack"))
    print(best("NY", "hert attack"))
}