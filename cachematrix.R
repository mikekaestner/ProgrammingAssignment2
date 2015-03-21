## makeCacheMatrix returns a list of functions to set and get the matrix
## and set and get the inverted matrix to resp from cache.

makeCacheMatrix <- function(x = matrix()) {
  # Initialize cache
  inverted <- NULL
  # Set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # Get the matrix.
  get <- function() x
  # Set the inverted matrix.
  setInverse <- function(inverse) inverted <<- inverse
  # Get the inverted matrix.
  getInverse <- function() inverted
  # Return the list of functions.
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# cacheSolve inverts the matrix or returns it from cache if it already exists.
cacheSolve <- function(x, ...) {
  inverted <- x$getInverse()
  # If the inverted matrix already exists, return it.
  if (!is.null(inverted)) {
    message("getting cached inverted matrix")
    return(inverted)
  }
  # If the matrix is not yet inverted, do it.
  matrix <- x$get()
  inverted <- solve(matrix, ...)
  # Cache the inverted matrix and return it.
  x$setinv(inverted)
  inverted
}