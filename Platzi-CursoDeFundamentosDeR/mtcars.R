#Consult dataset structure
str(mtcars)

#Get Help about this dataset
?mtcars

#Consult the datum type
class(mtcars$vs)
class(mtcars$am)

#Convert the datum type
mtcars$vs = as.logical(mtcars$vs)
mtcars$am = as.logical(mtcars$am)