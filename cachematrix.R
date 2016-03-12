## These functions calculate and cache the inversion of a matrix.
## If the inverse has already been calculated and cached, the 
## fucntion will returned the cached value instead of calculating
## it again.

## Use example:
## source("cachematrix.R")
## 

## makeCacheMatrixConstructs a list object that embedds functions for
## setting and getting a matrix and its inverse.  The inverse is
## cached globally.

makeCacheMatrix <- function(x = matrix()) {
  
  invMatrix <- NULL
  
  ## set the input to the global environment
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  
  ## return the matrix
  get <- function() x
  
  ## set the inverse matrix
  setInvMatrix <- function(solve) invMatrix <<- solve
  
  ## return the inverse matrix
  getInvMatrix <- function() invMatrix
  
  
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}


## Function returns the inverse of a matrix 
## It first checks for a cached value before calculating again
## Inverse is caclulated using solve()

cacheSolve <- function(x, ...) {
  ## Get the inverted matrix
  invMatrix <- x$getInvMatrix()
  
  ## If the inverted matrix has already been calculated
  ## then the value from the get (calcInv) will be 
  ## Not Null and the matrix can be returned rather than
  ## calculated again
  if(!is.null(invMatrix)) {
    message("Getting cached data:")
    return(invMatrix)
  }
  
  ## The inverted matrix hasn't been calculated
  ## get the base matrix
  inMatrix <- x$get()
  
  ## Calculate the inverse
  calcInvMatrix <- solve(inMatrix)
  
  ## Cache the result
  x$setInvMatrix(calcInvMatrix)
  
  ## Return a matrix that is the inverse of 'x'
  calcInvMatrix
  
}
