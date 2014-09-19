# Two functions that work together to calculate and store 
# the inverse of a matrix.
#the matrix is assumed to be square and invertible, 
# if not the solve() method will give error.

# inputs a matrix and returns a wrapper around the matrix.
# the output of this function can be passed into the cacheSolve
# expose 4 methods
#  get - retrns the original matrix
#  set - sets the original matrix
#  getinv - gets the inv (NULL if not yet stored)
#  setinv - sets the inv.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(mean) m <<- mean
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# this function inputs a "cacheMatrix" output from makeCacheMattrix
# and returns the inverse of the underlying matrix.  it caches the inverse, 
#  so if called again, tis method returns with inv without calling solve. 
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  # we could check m is square and invertible, but 
  # we can just let solve() give an error
  m <- solve(data, ...)
  x$setinv(m)
  m
}
