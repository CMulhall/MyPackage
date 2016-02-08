#' Creates object of class "fracTrac"
#' @description Takes a data.frame and converts it to class facTrac and then calls summary on
#' the newly created object to print the relevant information.
#' @param x - must be a data.frame
#' @return returns the summary of a facTrac class object through an internal call to summary
#' @examples
#' x <- system.file("extdata", "FacultyInfo.csv", package = "MyPackage")
#' createfacTrac(x)
#'@export
createfacTrac <- function(x){
  if (!is.data.frame(x)){
    print("Only objects of class data.frame can be converted to objects of class facTrac. Please ensure that your input is of class data.frame.")
  }else{
    # for both oldest and youngest, I'm returning the first alphabetically
    # with the respective highest and lowest ages.
    tempdata <- subset(x, x$Year.Of.Degree == min(x$Year.Of.Degree))
    tempdata <- droplevels(tempdata)
    oldest <- tempdata[1]
    tempdata <- subset(x, x$Year.Of.Degree == max(x$Year.Of.Degree))
    tempdata <- droplevels(tempdata)
    youngest <- tempdata[1]
    datasize <- nrow(x)

    # rounding to 2 decimal places to keep it clean
    meanAge <- round(mean(x$Age), 2)

    avAge <- meanAge
    oldFac <- oldest$Name
    youngFac <- youngest$Name
    numFac <- datasize

    result <- c(avAge, oldFac, youngFac, numFac)

    class(result) <- "facTrac"
    result
    }
}
