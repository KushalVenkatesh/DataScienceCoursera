corrSub <- function(directory, id) {
    fileName <- paste(formatC(id, width = 3, flag = "0"), ".csv", sep = "")
    filePath <- paste(directory, "/", fileName, sep = "")
    frame <- read.csv(filePath)
    na.omit(frame)
}

corr <- function(directory, threshold = 0) {
    frames <- Map(function(threshold) corrSub(directory, threshold), 1:332)
    framesAboveThreshold <- Filter(function(frame) dim(frame)[1] > threshold, frames)
    correlationList <- Map(function(frame) { cor(frame$sulfate, frame$nitrate)}, framesAboveThreshold)
    correlations <- unlist(correlationList)
    if (length(correlations) > 0) correlations
    else vector("numeric", 0)
}

# Tests #
#dir <- "E:/DataScienceCoursera/specdata"
#head(corr(dir, 150))
#summary(corr(dir, 150))
#head(corr(dir, 400))
#summary(corr(dir, 400))
#summary(corr(dir, 5000))
#length(corr(dir, 5000))
#summary(corr(dir))
#length(corr(dir))