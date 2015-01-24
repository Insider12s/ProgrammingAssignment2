## Matrix inversion can be typically a slow operation. 
## This slow operation can be hindered ( even more )if the vector contents becomes long 
## and tedious.I have created a program that eliminate some of the burden of computing
## if these values can be cached, stored and recalled when needed instead of re-computing
## for this value each time. This makes me sad. :-(
## This Programming Assignment will use scoping rules of the R Programming language and 
## how it can be used to preserve the state inside of a R object.
 

## The first function, I call "makeCacheMatrix" creates a special "vector", which is really 
## a list containing a function to do the following.
## Set the value of the Matrix
## Get the value of the Matrix
## Set the value of the inverse of the Matrix
## Get the value of the inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {
  a<-NULL
  set<-function(y){
    x<<-y
    a<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) a<<- solve
  getmatrix<-function() a
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## This Second function calculates the inverse Matrix of the special "vector" created from the
## above function. It first checks to see if the inverse has already been calculated. If 
## it has... the value is not computed again but, pulled from cache. If the inverse is unknown 
## it is then calculated... The inverse matrix value is now "known" and does not 
## need to recalculated if the value (inverse matrix) is called upon again.

cacheSolve <- function(x=matrix(), ...) {
  a<-x$getmatrix()
  if(!is.null(a)){
    message("Getting cached data")
    return(a)
  }
  matrix<-x$get()
  a<-solve(matrix, ...)
  x$setmatrix(a)
  a
}