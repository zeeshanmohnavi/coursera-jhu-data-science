## Calculate the inverse of an invertible square matrix and cache 
## the inverse to avoid recalculation of the inverse (for the same matrix).
## It uses the lexical scoping rules of R to create and access the matrices.

## Create a matrix object x and store its inverse.
## Specifically, create the setter and getter functions to access x
## and the inverse of x.
## Return value: a named list object containing the setter
## and getter functions.

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


## Return a matrix that is the inverse of 'x'
## It takes in an argument of type makeCacheMatrix and, using the
## getter function getinverse(), retrieves the inverse of matrix 'x'.
## If the inverse is found, return the inverse.
## Otherwise, calculate the inverse of the matrix and store the 
## calculated inverse in the cache using the setinverse()
## function of the makeCacheMatrix object.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    print("Getting cached data")
    return(inverse)
  }
  mat <- x$get()
  inverse <- solve(mat, ...)
  x$setinverse(inverse)
  inverse
}
