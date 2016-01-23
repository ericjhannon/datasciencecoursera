## The makesCacheMatrix and cacheSolve functions work together to save
## uneccesary processing by caching the result of a matrix inversion and
## returning it to the calling function if it has already been calculated

## The makeCacheMatrix function retrieves and sets the matrix and inverted matrix
## in the main function, so that it is maintained in memory during ongoing processing

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function firsts checks cache to see if the inverse of matrix x
## has already been calculated. If yes, the inverse currently in cache is
## returned. If not the inverse is calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
