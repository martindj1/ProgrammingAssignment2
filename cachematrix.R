## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix has four functions

## 1. Set the value of the vector
## 2. Get the value of the vector
## 3. Set the value of the mean
## 4. Get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve will compute the inverse of the matrix created above. If the inverse has already been calculated, then the function should retrive the inverse from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
