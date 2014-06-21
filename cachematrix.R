## These functions work in tandem to cache and calculate the inverse of a matrix.

## makeCacheMatrix takes a matrix and returns a function that encapsulates the
## matrix and the matrix's calculated inverse. The returned function allows
## setting and getting a value for the matrix via get/set as well as the inverse
## matrix via get/setinverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve takes a function created by makeCacheMatrix and returns the inverse
## of the matrix held by makeCacheMatrix. Subsequent calls to cacheSolve return
## the cached inverse of the matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  if(!is.null(inv)){
    message("returning cached inverse")
    return(inv)
  }
  
  inv <- solve(x$get())
  x$setinverse(inv)
  inv
}
