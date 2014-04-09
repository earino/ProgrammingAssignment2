## library to provide matrix inversion caching
## 
## software provides two functions to be used in concert for the purposes
## of wrapping a matrix object with an adaptor that memoizes inversion calls.
##
## uses: to be used in cases where a large matrix must be inverted multiple times,
##       or if the inversion happens inside of a loop
##

## makeCacheMatrix
## 
## Description
## Wraps a matrix object in a list with getter/setters for matrix inversion. this
## function should be called when preparing a matrix for use with the cacheSolve 
## function.
##
## Usage
## tt <- matrix( rnorm(3000*3000,mean=0,sd=1), 3000, 3000)
## cachingMatrix <- makeCacheMatrix(tt)
##
## Arguments
## 
## x     the matrix to be prepared for cached matrix inversion
##
## Returns
##
## a list with the keys set, get, setsolve, and getsolve
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve) 
}


## cacheSolve
## 
## Description
## When provided a matrix prepared by makeCacheMatrix, calls
## the native solve function to execute matrix inversion. If
## the matrix is inverted again, a memoized result is provided
## thereby accelerating response time.
##
##
## Usage
## tt <- matrix( rnorm(3000*3000,mean=0,sd=1), 3000, 3000)
## cachingMatrix <- makeCacheMatrix(tt)
## inverseOfMatrix <- cacheSolve(cachingMatrix) 
##
## Arguments
## 
## x     the object provided by makeCacheMatrix
##
## Returns
##
## the inverse of the original matrix, back in a native R matrix
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
