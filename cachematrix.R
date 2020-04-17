## The first function 'makeCacheMatrix' accepts a matrix and then defines and creates a list of functions 
## that store and retrieve matrix data.The second function "cacheSolve' calculates the inverse of a 
## matrix stored in the first function.


## makeCacheMatrix stores the supplied matrix in x either by a call to the main function or the nested 
## function 'set' (which uses the <<- operator to cache the data in the parent environment to be
## available to the get function - 'setInverse' does the same with the inverse supplied by 
## cachesolve for 'getInverse')
## Functions are given names via 'list' so they can be called with $.

makeCacheMatrix <- function(x = matrix()) {
  cached_inv <- NULL        # initialize for first run and clear for subsequent calls
  
  set <- function(latestMatrix) {
    x <<- latestMatrix
    cached_inv <<- NULL
  } 
  
  setInverse <- function(mInverse) {
    cached_inv <<- mInverse
  }
  
  
  get <- function() {x}
  
  getInverse <- function() {cached_inv}
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## Accepts a makecCacheMatrix type argument, checks to see if the inverse of the matrix is stored
## in makeCacheMatrix: if not it calculates, sends to makeCacheMatrix and prints; Else the cached 
## value is retrieved.

cacheSolve <- function(x, ...) {
  cached_inv <- x$getInverse()
  if(is.null(cached_inv)) {
    mx <- x$get()
    cached_inv <- solve(mx)
    x$setInverse(cached_inv)
    cached_inv
  }
  else {return(cached_inv)}
}        