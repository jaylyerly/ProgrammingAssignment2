##
##  This file contains two functions:
##      makeCacheMatrix
##      cacheSovle
##
##  These two functions can be used together to 
##  compute the inverse of a matrix and cache the
##  result so that subsequent calls for the inverse
##  of the same matrix return the cached copy, 
##  rather than recomputing the inverse.
##
##  For example:
##
##      x = replicate(10, rnorm(10))
##      xCache = makeCacheMatrix(x)
##      xInverse = cacheSolve(xCache)
##      ....
##      xInverse = cacheSolve(xCache)
##
##  The inverse of x is calculated on the first 
##  call to cacheSolve(xCache).  Future callse
##  to cacheSolve(xCache) return the cached 
##  value and do not incur the expense of solving
##  the matrix inverse again.
##

###################################################
##
## function: makeCacheMatrix
##
## arguments: 
##    x -- an invertible matrix 
##
## return value: a list containing the following
##    getter/setter functions:
##        set(x) -- set the matrix and clear the cache
##        get() -- return the matrix
##        setinverse(i) -- set the cached value of the
##              inverse of the matrix
##        getinverse() -- return the cached inverse
##              of the matrix. Returns NULL if there
##              is no cached value
##
## description:  This function takes a matrix and 
##      returns a list of functions which allow 
##      for caching the value of the matrix's 
##      inverse.
##
###################################################

makeCacheMatrix <- function(x = matrix()) {
    # set the cached inverse to NULL
    imatrix <- NULL
    
    # create the setter function for the matrix
    set <- function(y) {
        x <<- y            # store the matrix
        imatrix <<- NULL   # reset the cache to NULL to 
                           # insure we recalculate the inverse
    }
    
    # getter function for the matrix
    get <- function() x
    
    # setter function for the inverse matrix
    setinverse <- function(inverse) imatrix <<- inverse
    # getter function for the inverse matrix
    getinverse <- function() imatrix
    
    # return a list of the four getter/setter functions
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)

}

###################################################
##
## function: cacheSolve
##
## arguments: 
##    x -- a list created by the makeCacheMatrix
##         function which stores a matrix and a cache
##         of it's inverse
##    ... -- argument list that is passed through to 
##           the solve() function
##
## return value: the inverse of the matrix stored
##       in the input list as x$get
##
## description:  This function takes the list output of 
##      the makeCacheMatrix and returns the inverse of 
##      the matrix passed to makeCacheMatrix.  The 
##      inverse of the matrix is cached so it is only
##      calculated on the first call.  Subsequent 
##      calls return the cached value of the inverse
##      matrix.
##
###################################################
cacheSolve <- function(x, ...) {
    
    # retrieve the current value of the inverse (may be null)
    imatrix <- x$getinverse()
    
    # if the cached inverse is not null, return the cached value
    if(!is.null(imatrix)) {
        message("getting cached data")  # inform user about caching
        return(imatrix)
    }
    
    # if there is no cached value, retrieve the oringal
    # matrix in order to compute the inverse
    data <- x$get()
    
    # compute the inverse of the matrix
    imatrix <- solve(data, ...)
    
    # store the computed inverse in the cache
    x$setinverse(imatrix)
    
    # return the computed inverse of the matrix
    imatrix
}
