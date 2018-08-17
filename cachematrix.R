## R Programming, Assignment 2: write a pair of functions that can cache the inverse of a matrix

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      invMatrix <- NULL
      set <- function(y){
            x <<- y
            invMatrix <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) invMatrix <<- inverse
      getInverse <- function() invMatrix
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the matrix returned by makeCacheMatrix function

cacheSolve <- function(x, ...) {
      invMatrix <- x$getInverse()
      if(!is.null(invMatrix)) {
            message("getting cached data")
            return(invMatrix)
      }
      data <- x$get()
      invMatrix <- solve(data, ...)
      x$setInverse(invMatrix)
      invMatrix
}
