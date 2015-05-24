## makeCacheMatrix this function returns
## a list containing functions to set and to get the matrix x;
## and also set and get the inverse
## this list should be used as the input to cacheSolve() function

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(newMatrix) { 
    x <<- newMatrix
    invMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invMatrix <<- inverse 
  getInverse <- function() invMatrix
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}



## cacheSolve function returns the inverse of the matrix input to 
## makeCacheMatrix()

cacheSolve <- function(x, ...) {
  invMatrix <- x$getInverse()
  
  if (!is.null(invMatrix)){
    message("getting cached data")
    return(invMatrix)
  }
  
  matrixData <- x$get()
  invMatrix <- solve(matrixData, ...)
  
  x$setInverse(invMatrix)
  
  invMatrix
}
