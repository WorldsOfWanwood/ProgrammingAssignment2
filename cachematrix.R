# makeCacheMatrix creates a matrix object with a cache of its inverse 
# (the value of which is null if the inverse has not yet been calculated)
# cacheSolve returns the inverse of a matrix - if the inverse has already been
# calculated, it return that, otherwise it calculates the inverse
# and caches it in the matrix object.

# Takes a matrix, and returns a list of functions that can be used to 
# return the inverse ( getInverse() ), return the matrix ( get() ),
# set the inverse ( setInverse(newInverse) ),
# or set a new matrix which will reset the cached inverse to NULL ( set(newMatrix) )

makeCacheMatrix <- function(matrix = matrix()) {
  inverse <- NULL
  set <- function(newMatrix){
    matrix <<- newMatrix
    inverse <<- NULL
  }
  get <- function(){
    matrix
  }
  setInverse <- function(newInverse) {
    inverse <<- newInverse
  }
  getInverse <- function() {
    inverse
  }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)  
}


# cacheSolve takes a matrix object as created with makeCacheMatrix, 
# and returns the inverse of a matrix, either from the cached version in the object
# or if that is null, using the usual solve function (it then caches the calculated inverse)

cacheSolve <- function(matrix, ...) {
  inverse <- matrix$getInverse()
  if(is.null(inverse)) {
    inverse <- solve(matrix$get(), ...)
    matrix$setInverse(inverse)
  }  
  inverse
}