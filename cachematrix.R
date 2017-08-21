## The function below implements a chacheable matrix by storing the matrix content in environment and 
## returning a list of four methods through with the matrix content and its inverse can be read or written

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inverseMatrix <<- inverse
  getInverse <- function() inverseMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The function cacheSolve implements the cached inverse for the above makeCacheMatrix function
## It first invokes the getInverse method to determine if a chached inverse is available
## If so it will return the cached inverse matrix. If it is not available it will use
## the solve() function to calculate the inverse and store it in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInverse()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  # cached inverse is not availble; calculate and cache it
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  x$setInverse(inverseMatrix)
  inverseMatrix
}

## this test the above functions

## Create a 2x2 cached matrix:
set.seed(12345)
a=matrix(data=runif(4),nrow=2,ncol=2)
a2=makeCacheMatrix(a)

## Get the matrix:
a2$get()

## Get the cached inverse (not available yet, it will calculate and store it):
cacheSolve(a2)

## And again, this time it will use the cached result:
cacheSolve(a2)

## For comparison: direct calculation of the inverse on the original matrix:
solve(a)
