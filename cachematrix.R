## These pair of functions compute and cache the inverse of a matrix inside a special object
## makeCacheMatrix creates a special "matrix" object that can store the inverse of the matrix
## cacheSolve retrieves the value of the matrix inverse from cache or computes and caches
## the inverse.

## This function creates an object that is a list of functions to set and retrieve the matrix 
## and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ## variable inv caches matrix inverse
  set <- function(y) {
    x <<- y  ## Stores the matrix in variable x within makeCaheMatrix environment
    inv <<- NULL ## Resets inverse 
  }
  get <- function() x ## Retrieves the matrix
  setinverse <- function(inverse) inv <<- inverse ## Caches the inverse
  getinverse <- function() inv ## Returnes cached inverse value
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the matrix stored in the object returned by makeCacheMatrix 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv ## Return a matrix that is the inverse of 'x'
}
