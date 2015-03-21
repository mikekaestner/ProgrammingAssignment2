## makeCacheMatrix returns a list of functions to 1. set and 2. get the matrix
## and 3. set and 4. get the inverted matrix to respectively from cache.

makeCacheMatrix <- function(x = matrix()) {
  # Initialize cache
  inverted <- NULL
  # 1. Set the matrix
  set <- function(y) {
    x <<- y
    inverted <<- NULL
  }
  # 2. Get the matrix.
  get <- function() x
  # 3. Set the inverted matrix.
  setInverse <- function(inverse) inverted <<- inverse
  # 4. Get the inverted matrix.
  getInverse <- function() inverted
  # Return the list of functions.
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# cacheSolve inverts the matrix or returns it from cache if it already exists.
cacheSolve <- function(x, ...) {
  inverted <- x$getInverse()
  # If the inverted matrix already exists, return it.
  if (!is.null(inverted)) {
    message("Your matrix is already inverted. Let me fetch it from the cache for you.")
    return(inverted)
  }
  # If the matrix is not yet inverted, do it.
  matrix <- x$get()
  inverted <- solve(matrix, ...)
  # Cache the inverted matrix and return it.
  x$setInverse(inverted)
  inverted
}