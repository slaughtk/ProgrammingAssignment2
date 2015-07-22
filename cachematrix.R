## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Function makeCacheMatrix() creates an environment consisting of, for the matrix
##   x passed into the function as its argument, 1) matrix functions 
##   set(), get(), setinverse(), and getinverse(), as well as matrix imtr,
##   which will contain the cached inverse of x.

## makeCacheMatrix() returns a 4-column vector consisting of references to the
## functions created in the environment so that the functions can be called
## from the Global environment

makeCacheMatrix <- function(x = matrix()) {
  
  ## Create imtr as an  empty matrix
  
  imtr <- matrix(numeric(), nrow=0, ncol=0)
  
  ## Function set() allows matrix x to be reset.  When it is reset, matrix imtr
  ## no longer contains the inverse of x, so imtr is reset to be an empty matrix
  
  set <- function(y) {
    x <<- y  ## `<<-` facilitates resetting variables defined in the parent
             ## environment, in this case the makeCacheMatrix() environment
             ## associated with the matrix argument x
    imtr <<- matrix(numeric(), nrow=0, ncol=0)
  }
  
  ## Function get() returns the matrix x originally passed into function
  ## makeCacheMatrix as the argument
  
  get <- function() x
  
  ## Function setinverse() assigns to matrix variable imtr the matrix argument imtrx,
  ## which should be the inverse of the original matrix x.  This has the effect
  ## of caching the inverse of x in imtr
  
  setinverse <- function(imtrx) imtr <<- imtrx
  
  ## Function getinverse() returns the matrix imtr which, if not empty, should be
  ## the inverse of the original matrix x
  
  getinverse <- function() imtr
  
  list(set = set, get = get,    ## set a list of all four functions and return it
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

## Function cacheSolve() returns the inverse of the original matrix x passed
## as the argument to makeCacheMatrix().
##
## The 1st argument x to cacheSolve() is a vector with four columns "set," "get," 
## "setinverse," and "getinverse," which are references to the eponomous functions
## (related to the original matrix x) created in function makeCacheMatrix().

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## Call function getinverse() to retrieve the cached inverse of original matrix x
  ivrs <- x$getinverse()
  ## if the matrix returned is nonempty, it is the cached inverse of orig. matrix x
  if(length(ivrs) > 0) {  
    message("getting cached data")
    return(ivrs) ## return the cached inverse
  }
  mtrxdata <- x$get()  ## retrieve the original matrix x by calling get()
  ivrs <- solve(mtrxdata, ...) ## calc. its inverse, passing along any arguments

  x$setinverse(ivrs) ## cache the matrix inverse by calling setinverse()

  ivrs ## return the calculated inverse

}
