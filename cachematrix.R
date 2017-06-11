## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function creates a special "matrix" object that 
## can cache its inverse.
## It supports i) setting & getting matrix, ii) setting & getting inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invers) inv <<- invers
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The following function namely "cacheSolve" function
## computes the inverse of the special "matrix" 
## returned by "makeCacheMatrix" function above. If the inverse has already
## been calculated (and the matrix has not changed), then the
## cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}



