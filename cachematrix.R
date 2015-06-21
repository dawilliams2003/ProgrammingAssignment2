## Put comments here that give an overall description of what your
## functions do

## Dave Williams, DataScienceSpecialization, R Programming, Assignment 2
## 6/21/15
## Create two function objects that return lists of functions.
## Their purpose is to store a matrix in an object that can also store
## the inverse of the matrix. When the underlying data of the object
## (the actual matrix) is changed, the inverse is recalculated.
## When requesting the inverse, the cache will be used if it is available.


makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverted) inverse <<- inverted
  getinverse <- function() inverse
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes an object of type makeCacheMatrix.
## If the inverse is already calculated, this simply returns
## the cached inverse and alerts the user of that.
## Otherwise, it callses solve(matrix) and sets the internal
## inverse value of the object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
