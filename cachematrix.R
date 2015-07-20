## R Programming -- Assignment 2
## Function to cache the inverse of a matrix to avoid re-computing the inverse
## If the inverse does not exist, it is computed.  If it exists, it is copied.
##----------------------------------------------------------------------------
##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y=matrix()) {
    x <<- y
    xinv <<- NULL
  }
  get <- function() x
  setinv <- function(xinv) xinv <<- x
  getinv <- function() xinv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}

##----------------------------------------------------------------------------
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xinv <- x$getinv()
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
  data <- x$get()
  xinv <- solve(data)
  x$setinv(xinv)
  xinv
}
