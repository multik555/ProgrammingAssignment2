## Put comments here that give an overall description of what your
## functions do:
## Create the functions which help cache of inverse of a matrix.

## Write a short comment describing this function:
## Assign special variable as a function that can cache inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## Write a short comment describing this function:
## Compute the inverse of the special matrix returned by "makeCacheMatrix".
## If the inverse has already been calculated, then the "cacheSolve" 
## should extract the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}