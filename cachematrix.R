## This script named "cachematrix" contains two functions that will creates a special
##  "matrix" object that can cache its inverse and then compute the inverse of the special
##  matrix.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      mat <- NULL
      set <- function(y) {
            x <<- y
            mat <<- NULL
      }
      get <- function() x
      setsolved <- function(solved) mat <<- solved
      getsolved <- function() mat
      list(set = set, get = get,
           setsolved = setsolved,
           getsolved = getsolved)
}

## chaceSolve:  This function computes the inverse of the special "matrix" returned by 
##  makeCacheMatrix above. If inverse has already been calculated (and the matrix 
##  has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      mat <- x$getsolved()
      if(!is.null(mat)) {
            message("Getting Cached Matrix")
            return(mat)
      }
      data <- x$get()
      mat <- solve(data, ...)
      x$setsolved(mat)
      mat
}
