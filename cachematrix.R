## Creating a Special matrix and caching its inverse:
## Below are the two fuctions makeCacheMatrix and cacheSolve.
## makeCacheMatrix function creates a special matrix that can cache its inverse
## and cacheSolve function computes the inverse of the special matrix created by
## the makeCacheMatrix function.


## This function creates a special matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix"
## returned by `makeCacheMatrix` above. If the inverse has already 
## been calculated (and the matrix has not changed), then
## cacheSolve` should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}
  
source("cachematrix.R")
matrix<-makeCacheMatrix(matrix(c(1,2,4,3,2,3,4,6,5),3,3))
matrix$get()
matrix$getinverse()
cacheSolve(matrix)

matrix<-makeCacheMatrix(matrix(1:4,2,2))
matrix$get()
matrix$getinverse()
cacheSolve(matrix)


