completeSub <- function(directory, id) {
    fileName <- paste(formatC(id, width = 3, flag = "0"), ".csv", sep = "")
    filePath <- paste(directory, "/", fileName, sep = "")
    frame <- read.csv(filePath)
    obsClean <- na.omit(frame)
    obsCleanCount <- nrow(obsClean)
    list(id = id, nobs = obsCleanCount)
}

complete <- function(directory, id = 1:332) {
    completeLists <- Map(function(id) completeSub(directory, id), id)
    matrix <- do.call(rbind, completeLists)
    as.data.frame(matrix)
}

# Tests #
#dir <- "E:/DataScienceCoursera/specdata"
#complete(dir, c(2, 4, 8, 10, 12))
#complete(dir, 30:25)
#complete(dir, 3)