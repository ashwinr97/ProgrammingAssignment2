## Put comments here that give an overall description of what your
## functions do

## This function is used to create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
  
}


## This function is used to compute the inverse of the special matrix created above
## If the inverse has already been created then it will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  
  
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) %*% data
  x$setInverse(m)
  m
  ## Return a matrix that is the inverse of 'x'
}
