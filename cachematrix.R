## This code provides a pair of functions that compute and 
## and cache the inverse of a matrix, so that on subsequent calls
## the inverse of the matrix can be returned from the cache and avoid
## costly computations.
## Assumption for this code: the matrix supplied is always invertible

## makeCacheMatrix : this function creates a special "matrix" object 
## that caches its inverse.
## input type: matrix
## output type: object
##
## The returned matrix object provides the following functions:
## set: set the value of the vector
## get: get the value of the vector
## setsolve: set the value of the mean
## getsolve: get the value of the mean
## 


makeCacheMatrix <- function(x = matrix()) {
  inversematrix <- NULL
  set <- function(y) {
    x <<- y
    inversematrix <<- NULL
  }
  get <- function() x
  setsolve <- function(i) inversematrix <<- i
  getsolve <- function() inversematrix
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## cacheSolve:  this function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the cacheSolve 
## retrieves the inverse from the cache.
##
## input type: matrix
## output type: matrix


cacheSolve <- function(x) {
  ## Returns a matrix that is the inverse of 'x'
  inversematrix <- x$getsolve()
  if(!is.null(inversematrix)) {
    message("getting cached data")
    return(inversematrix)
  }
  data <- x$get()
  inversematrix <- solve(data)
  x$setsolve(inversematrix)
  inversematrix
}


## example usage and output
## > test_matrix <- matrix(c(1/2, -1/4, -1, 1/4), nrow = 2, ncol = 2)
## > testMatrix_object <- makeCacheMatrix(test_matrix)
## > cacheSolve(testMatrix_object)
## [,1] [,2]
## [1,]   -2   -8
## [2,]   -2   -4
## > cacheSolve(testMatrix_object)
## getting cached data
## [,1] [,2]
## [1,]   -2   -8
## [2,]   -2   -4
