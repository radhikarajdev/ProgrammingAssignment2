## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # Initialize 'inv' to NULL to store the cached inverse of the matrix
  inv <- NULL
  
  # Setter function to assign a new matrix to 'x' and reset 'inv' to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Getter function to return the current matrix stored in 'x'
  get <- function() x
  
  # Setter function to cache the inverse matrix in 'inv'
  setInverse <- function(inverse) inv <<- inverse
  
  # Getter function to retrieve the cached inverse matrix from 'inv'
  getInverse <- function() inv
  
  # Return a list of functions to interact with the matrix and its inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse is already cached and the matrix hasn't changed, it retrieves the cached inverse.
cacheSolve <- function(x, ...) {
  # Retrieve the cached inverse if it exists
  inv <- x$getInverse()
  
  # If the inverse is cached, print a message and return the cached inverse
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If no cached inverse exists, retrieve the matrix from 'x'
  data <- x$get()
  
  # Compute the inverse of the matrix
  inv <- solve(data, ...)
  
  # Cache the computed inverse for future use
  x$setInverse(inv)
  
  # Return the newly computed inverse
  inv
}
