## CacheMatrix project with functions "makeCacheMatrix' and 'cacheSolve' 
## allows to create a matrix object and cache inverse matrix 

## makeCacheMatrix creates matrix object and stores in cache inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  
  ## Sets new matrix inside an object
  set <- function (y) {
    x <<- y
    x_inv <<- NULL
  }
  
  ## Returns matrix inside an object
  get <- function() x
  
  ## Saves in cache inverse matrix of 'x'
  setInverse <- function(inv) x_inv <<- inv
  
  ## Returns inverse matrix of 'x'
  getInverse <- function() x_inv
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Returns matrix that is inverse to x inside makeCacheMatrix object
## If inverse is saved in cache returns it from cache
cacheSolve <- function(x, ...) {
  x_inv <- x$getInverse()
  if (!is.null(x_inv)) {
    message("getting cached data")
    return(x_inv)
  }
  data <- x$get()
  x_inv <- solve(data, ...)
  x$setInverse (x_inv)
  x_inv
}
