## Matrix inversion is usually a costly computation and there 
## may be some benefit to caching the inverse of a 
## matrix rather than computing it repeatedly

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) im <<- solve
  getsolve <- function() im
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The following function calculates the inverse matrix of the special "matrix" 
## created with the above function. However, it first checks to see 
## if the inverse matrix has already been calculated. If so, it gets the inverse matrix 
## from the cache and skips the computation. Otherwise, it calculates 
## the inverse matrix of the data and sets the value of the inverse matrix in the cache 
## via the setsolve function.

cacheSolve <- function(x, ...) {
  im <- x$getsolve()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setsolve(im)
  im
}
