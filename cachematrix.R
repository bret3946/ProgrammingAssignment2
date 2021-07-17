## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it
## repeatedly. These functions cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  setMatrix <- function(value) {
    x <<- value
    # clear the inverse
    setInverse(NULL)
  }
  getMatrix <- function() {
    x
  }
  setInverse <- function(value) {
    i <<- value
  }
  getInverse <- function() {
    i
  }
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if (is.null(i)) {
    m <- x$getMatrix()
    i <- solve(m, ...)
    x$setInverse(i)
  }
  i
}

## This is the consolidation of the makeCacheMatrix and cacheSolve functions,
## to more closely resemble a "matrixCache" object.

matrixCache <- function(m) {
  i <- NULL
  setMatrix <- function(value) {
    m <<- value
    i <<- NULL
  }
  getMatrix <- function() {
    m
  }
  setInverse <- function(value) {
    i <<- value
  }
  getInverse <- function(...) {
    if (is.null(i)) {
      i <<- solve(m, ...)
    }
    i
  }
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}
