makeCacheMatrix <- function(x = matrix()) {
  fan <- NULL
  set <- function(y) {
    x <<- y
    fan <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) fan <<- inverse
  getInverse <- function() fan
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  fan <- x$getInverse()
  if (!is.null(fan)) {
    message("getting cached data")
    return(fan)
  }
  mat <- x$get()
  fan <- solve(mat, ...)
  x$setInverse(fan)
  fan
}
