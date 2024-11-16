# Create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # Initialize inverse as NULL
  
  set <- function(y) {
    x <<- y # Set the matrix
    inv <<- NULL # Reset the cached inverse
  }
  
  get <- function() x # Get the matrix
  
  setInverse <- function(inverse) inv <<- inverse # Cache the inverse
  
  getInverse <- function() inv # Retrieve the cached inverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Compute the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  inv <- x$getInverse() # Check if inverse is cached
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv) # Return cached inverse
  }
  
  mat <- x$get() # Retrieve the matrix
  inv <- solve(mat, ...) # Compute the inverse
  x$setInverse(inv) # Cache the inverse
  
  inv # Return the inverse
}

# Example usage
# Create a cacheable matrix
my_matrix <- makeCacheMatrix(matrix(c(2, 1, 1, 3), nrow = 2, ncol = 2))

# Compute the inverse (first time, it calculates)
inverse1 <- cacheSolve(my_matrix)
print(inverse1)

# Retrieve the cached inverse (second time, it fetches from cache)
inverse2 <- cacheSolve(my_matrix)
print(inverse2)


