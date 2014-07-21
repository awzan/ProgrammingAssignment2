## Training function from Programming Assignment 2.
## Its actual usefullness is doubtful, but never mind that!

## Pseudo-class "matrix"

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Function which solves the matrix for its inverse

CacheSolve <- function(x, ...) {
  ## Setting the matrix inverse "property" if and only if it is not already set
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
