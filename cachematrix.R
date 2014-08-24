## This function gets the inverse of a matrix, stores it, and then
## calls it in the future from the cache, if the same matrix is
## computed.

## This function creates a special matrix that holds the value of 
## the output from the function cacheSolve, allowing us to save
## computational steps. If the same matrix is inversed with
## cacheSolve, then we will just get the value from makeCacheMatrix
## that has the solution stored.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of a matrix, and sets the
## value of the inverse in the cache via the setinverse 
## function. Otherwise, it gets the value of the inverse from 
## makeCacheMatrix, if the matrix has been previously computed.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
