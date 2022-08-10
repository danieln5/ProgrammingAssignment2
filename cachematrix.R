## These functions compute and cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse
  i <- NULL
  ## Initialization when using an existing matrix to set 'x' 
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## Function to retrieve the matrix data
  get <- function() x
  ## Function to cache the inverse
  setinverse <- function(inverse) i <<- inverse
  ## Function to retrieve cached inverse
  getinverse <- function() i
  ## Create and return list data type special "matrix" object
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve retrieves the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  
  ## If the inverse of 'x' is already computed, return the cached inverse
  if(!is.null(i)) {
    message("Getting cached data")
    return(i)
  }
  
  ## Compute and cache the inverse of 'x'
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}



