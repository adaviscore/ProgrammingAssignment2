## makeCacheMatrix will create a new matrix and set the getter and setter functions
## cacheSolve will be given a List and then solve for the inverse of the Matrix. 
## It will first check to see if the data has been cached; if so, then go and 
## grab the data.

## This function takes a Matrix and sets it.
## When you call this function all the functions get called
## which then resets i and x appropriately.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {i <<- inverse}
  getInverse <- function() {i}
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

## cacheSolve will be given a List and then solve for the inverse of the Matrix. 
## It will first check to see if the data has been cached; if so, then go and 
## grab the data. Once solved it will cache it.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  m <- x$get()
  i <- solve(m, ...)
  x$setInverse(i)
  i
}