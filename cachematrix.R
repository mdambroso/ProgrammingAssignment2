## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  #Set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #Get the value of the matrix
  get <- function() x
  
  #Set the value of the inverse matrix
  setinverse <- function(inverse) inv <<- inverse
  
  #Get the value of the inverse matrix
  getinverse <- function() inv
  
  #Create list with results
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
 inv <- x$getinverse()
  
  #Checks to see if the mean has already been calculated
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  
  #Calculates the inverse of the data
  data <- x$get()
  inv <- solve(data)
  
  #Set the inverse
  x$setinverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
