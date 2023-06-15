
## For this assignment, assume that the matrix supplied is always invertible.

## 1. This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # inverse value
  inverse <- NULL
  # sets the original "x" matrix and resets inverse
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
      
  # gets the original matrix
  get <- function() x
  # sets the inverse value
  set_inverse <- function(inv) inverse <<- inv
  # gets the inverse value
  get_inverse <- function() inverse
  
  # Returns a list of the functions
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}
}


## Write a short comment describing this function

## 2. This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(special_m, ...) {
  inv <- special_m$get_inverse()
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  data <- special_m$get()
  inv <- solve(data, ...)
  special_m$set_inverse(inv)
  inv
}
