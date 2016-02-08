#' FacTrac - a way to subset data
#' @description Searches the faculty data set for a string that is either a full name, in 
#' which case the second argument should be 1, or is the title of a department in which 
#' case the second argument should be a 2. If you want the entire dataset, give the 
#' second parameter as 0.
#' @param a - String that will be searched for. Defaults to the empty string which will 
#' result in an empty dataset being returned.
#' @param b - Number (0, 1, or 2) 
#' @return Returns a the requested subset of the faculty data with a newly created age column.
#' @examples
#' facSearch()
#' @examples
#' a <- "American Studies"
#' b <- 2
#' facSearch(a,b)
#' @export

facSearch <- function(a = "",b = 0){
  facinfo <- read.csv(system.file("extdata", "FacultyInfo.csv", package = "MyPackage"), stringsAsFactors = FALSE)
  if (b == 0){
    result <- addAge(facinfo)
  }else if (b == 1){
    tempdata <- subset(facinfo, facinfo$Name == a)
    result <- addAge(tempdata)
  }else if (b == 2){
    #add a space due to a personal formatting error when making the orignal csv file
    tempdata <- subset(facinfo, facinfo$Department == paste(" ", a, sep = ""))
    result <- addAge(tempdata)
  }else{
    print("Please specify either 1 to search by Name, or 2 to search by 
          Department in the second paramter of facTrac(). Alternatively, you can use 
          no parameters and return the entire faculty data set.")
  }
  return(result)
}


#' Adds an Age Column to the data set
#' @description Creates an age column with the formula 2016 - Year.Of.Degree + 22, given
#' that we assume most B.A's are recieved at age 22.
#' @param x - a data.frame object to be modified
#' @return Returns the same data frame with an added Age column.
#' @examples 
#' x <- system.file("extdata", "FacultyInfo.csv", package = "MyPackage")
#' addAge(x)
#' @export

addAge <- function(x){
  if (!is.data.frame(x)){
    print("addAge only takes in objects of class data.frame.Please ensure that your 
          input matches this by checking with 'is.data.frame(yourinput)'")
  }else{
    for(i in 1:nrow(x)){
      x$Age <- 2016 - x$Year.Of.Degree + 22
    }
  }
  return(as.data.frame(x))
}
