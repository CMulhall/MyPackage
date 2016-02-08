#' Summary method along with the print method
#' @description Summarizes the given data set and creates a list with the average age of
#'  the faculty, the oldest, the youngest, and the number of faculty in the given dataset 
#'  and then prints these results in text.
#' @param object - an object of class facTrac that will be used to gather the information to calculate
#'  the summary results.
#' @param ... - this function does not take additional arguments at this time.
#' @return lines of text displaying the call made to create the summarized object as well as then
#'  printing the resulting summary statistics
#' @examples
#' x <- facSearch("Japanese",2)
#' object <- createfacTrac(x)
#' summary(object)
#' @export
summary.facTrac <- function(object, ...){
  old <- object[2]
  young <- object[3]
  summarystring <- paste("The average age is ", object[1], ".  ", 
                  "The oldest member is ", old, ".  ",
                  "The youngest member is ", young, ".  ",
                  "The number of faculty examined was ", object[4], ".  ", sep="")
  
  class(summarystring) <- "summary.facTrac"
  summarystring
}


print.summary.facTrac <- function(x){
  
  print(x$summarystring)
  
}