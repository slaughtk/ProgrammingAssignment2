## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  imtr <- matrix(numeric(), nrow=0, ncol=0)
  
  set <- function(y) {
    x <<- y
    imtr <<- matrix(numeric(), nrow=0, ncol=0)
  }
  
  get <- function() x
  setinverse <- function(imtrx) imtr <<- imtrx
  getinverse <- function() imtr
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ivrs <- x$getinverse()
  if(length(ivrs) > 0) {
    message("getting cached data")
    return(ivrs)
  }
  mtrxdata <- x$get()
  ivrs <- solve(mtrxdata, ...)
  x$setinverse(ivrs)
  ivrs

}
