## 
## These methods will create a matrix,
## calculate its inverse when commanded,
## and store the inverse in the cache
## to avoid recalculating

## The makeCacheMatrix function creates a
## matrix object with methods to access and
## manipulate its attributes
makeCacheMatrix <- function(x = matrix()) {
## By default the inverse is set to NULL
    inv <- NULL
  
## The set method assigns specified variable name to
## the value of its argument
    set <- function(y) {
      x <<- y
    inv <<- NULL
    }
## The get method returns the matrix associated
## with the specified variable name
  get <- function() x
  
## setInverse method stores a specified value as the inverse
## and stores it in cache
  setInverse <- function(inverse) inv <<- inverse

## The getInverse method pulls the (already calculated)
##  inverse of the matrix from the cache
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Calculates inverse of given matrix object and
## places it in cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
## if it has already been calculated
    inv <- x$getInverse()
    if(!is.null(inv)) {
      message("Getting cached data")
      return(inv)
    }
## Otherwise, use the 'get' method to calculate
## the inverse using 'solve' and store it in the cache
## using the setInverse method
    data <- x$get()
    inv <- solve(data,...)
    x$setInverse(inv)
## print the inverse
    inv
}