rankhospital <- function(state, outcome, num = "best") {
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
            outcomeData[, outcomeRatingColumnIndex] <- as.numeric(outcomeData[, outcomeRatingColumnIndex])
            outcomeDataOrdering <- order(outcomeData[, outcomeRatingColumnIndex], outcomeData$Hospital.Name)
            outcomeData <- outcomeData[outcomeDataOrdering, ]
            outcomeData <- outcomeData[complete.cases(outcomeData), ]
            outcomeDataRowCount <- nrow(outcomeData)
            num <- if (num == "best") 1 else if (num == "worst") outcomeDataRowCount else num
            if (outcomeDataRowCount < num) NA
            else outcomeData[num, ]$Hospital.Name
        }
    }
}

# Tests
test <- function () {
    print(rankhospital("TX", "heart failure", 4))
    print(rankhospital("MD", "heart attack", "worst"))
    print(rankhospital("MN", "heart attack", 5000))
}