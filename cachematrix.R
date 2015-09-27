# Creating a function that solves matrix inverse and saves it to cache: makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# The following function calculates the inverse of the special "matrix" created with the 
# above function. However, it first checks to see if the inverse has already been calculated.
# If so, it gets the inverse from the cache and skips the computation. Otherwise, it
# calculates the inverse of the data and sets the value of the inverse in the cache via the
# setinverse function.

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
