# Matrix inversion can be a costly computation so use caching to avoid recalculating the inverse when the data has not changed.

# makeCacheMatrix creates a list containing the following functions:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of inverse of the matrix
# 4. Get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL
  
  set <- function(y) {
    x <<- y
    # Clear the cache as new values are being set
    invMat <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) invMat <<- inverse
  
  getinverse <- function() invMat
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The following function returns the inverse of the matrix.
# It checks to see if the inverse has already been calculated.
# o If so it returns the cached values.
# o If not it computes the inverse and caches it.
cacheSolve <- function(x, ...) {
  invMat <- x$getinverse()
  
  if(! is.null(invMat)) {
    message("Returning cached data.")
        return(invMat)
    }

    data <- x$get()
    invMat<- solve(data)
    x$setinverse(invMat)

    invMat
}

