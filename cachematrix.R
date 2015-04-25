## These two functions solved assignment 2 from course of R Programming
## of Johns Hopkins University.

## This function The creates a special variable ables to set the value of the matrix,
## get the value of the matrix, set the value of the inverse and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the inverse of the original matrix
## created with the above function. It first checks to see if the inverse
## has already been calculated. If so, it gets the inverse from the cache 
## and skips the computation. Otherwise, it calculates it and sets the value 
## in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}