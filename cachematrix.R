## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse cache
  
  # Set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the inverse cache when matrix changes
  }
  
  # Get the matrix
  get <- function() {
    x
  }
  
  # Set the inverse of the matrix
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  # Get the cached inverse
  getInverse <- function() {
    inv
  }
  
  # Return a list of functions to access the matrix and its inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Function to compute the inverse of the matrix created by makeCacheMatrix
cacheSolve <- function(x, ...) {
  # Check if the inverse is already cached
  inv <- x$getInverse()
  
  # If the inverse is already cached, return it
  if (!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  
  # If not cached, get the matrix and compute its inverse
  mat <- x$get()
  inv <- solve(mat, ...)
  
  # Cache the inverse for future use
  x$setInverse(inv)
  
  # Return the computed inverse
  inv
}

