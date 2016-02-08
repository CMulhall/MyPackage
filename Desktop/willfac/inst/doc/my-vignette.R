## ------------------------------------------------------------------------
library(MyPackage)
allfaculty <- facSearch()
facobject <- createfacTrac(allfaculty)
summary(facobject)

## ------------------------------------------------------------------------
mathfaculty <- facSearch("Mathematics", 2)
mathfac <- createfacTrac(mathfaculty)
summary(mathfac)

