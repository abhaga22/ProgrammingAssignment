## This function creates a matrix. The set function links cacheSolve and makeCache Matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  } 
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
## Finds the inverse of the matrix and stores it.

## This method solves for the inverse by asking for a matrix input.
cacheSolve <- function(x, ...) {
  m <- x$getinverse() ## getting the inverse if it already present
  if (!is.null(m)){   ## checking for cache
    message("getting cached data")
    return(m)
  }
  data <- x$get() 
  m <- solve(data,...) ## If no cache then recalculate inverse
  x$setinverse(m)
  m ## Return a matrix that is the inverse of 'x'
}
