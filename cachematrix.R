## These are a set of two function that compute the inverse of a matrix
## and cache the result for subsequent uses
##
## Exemple of use :
## > mx <- matrix(data=1:4, nrow=2)
## > mx
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > cacheMatrix <- makeCacheMatrix(mx)
## > cacheSolve( cacheMatrix )
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5


## return a special matrix object that can cache its inverse
## INPUT
## x : a matrix
## RETURN a list of functions allowing access to the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL # the cached inverse matrix

  # next we define 4 functions that will allow to access the matrix and its inverse
  
  # set the matrix and reset the cache
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # get the matrix
  get <- function() x
  # set the cached matrix
  setinv <- function(inv) inv <<- inv
  # get the cached matrix
  getinv <- function() inv

  # Return a list of the previously defined functions. 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Compute the inverse of a matrix, cache it if not already cached, and return it
## INPUT
## x : the matrix object to be inversed
## RETURN a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {

  inv <- x$getinv()

  if(!is.null(inv)) { # if the inverse as already been calculated
    
    # nothing to do except inform the user
    message("getting cached data")
    
  } else {
    
    # else compute and cache the inverse 
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    
  }
  
  # return the inverse
  inv
  
}
