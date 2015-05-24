## functions to implement a cached matrix inverse
## first use the makeCacheMatrix() function to create
## a cache matrix object
## next call cacheSolve() function with the cache object
## to find the inverse of the matrix. It will calculate
## the inverse the first time, but subsequent calls
## will just return a cached copy of the inverse matrix.
## Example:
##      m <- matrix( c(1,1,0,1), 2, 2) # original matrix
##      cached_m <- makeCacheMatrix( m )
##      # first time, calculates inverse and saves it
##      inv_m <- cacheSolve( cached_m )
##      # ...
##      # next time, it just returns the cached copy of inverse
##      inv_m <- cacheSolve( cached_m )
##
##      n <- matrix( c(1,0,0,1), 2, 2) # a different matrix
##      cached_m$set(n) # original matrix changed to n
##      inv_n <- cacheSolve( cached_m ) # calculates inverse matrix for n
##      # ...
##      inv_n <- cacheSolve( cached_m ) # next time it uses the cached inverse
## makeCacheMatrix -- creates a cache matrix object
## that can be used to store both the original matrix
## and it's inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    # initially no inverse matrix
    invm <- NULL
    set <- function(y) {
        # reset the original matrix
        x <<- y
        # erase any previous cached inverse matrix
        invm <<- NULL
    }
    get <- function() x
    # store the inverse matrix
    setinv <- function(m) invm <<- m
    # get the saved inverse matrix
    getinv <- function() invm
    # returns a list of functions to
    # get, set the original matrix
    # get, set the saved inverse matrix
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)  
}


## cacheSolve -- computes inverse of a matrix, x
## it caches the inverse matrix once it has been calculated
## if the original matrix has not changed,
## subsequent calls to cacheSolve will save calculation time
## by simply returning the cached inverse matrix

cacheSolve <- function(x, ...) {
    ## x is an object created with makeCacheMatrix
    m <- x$getinv()
    if(!is.null(m)) {
      # yes, we do have a cached inverse matrix
      return(m)
    }
    # cached inverse matrix has not been calculated until now
    # get the matrix whose inverse is to be found
    data <- x$get()
    # find it's inverse
    m <- solve(data, ...)
    # save it in the cache for next time
    x$setinv(m)
    m
}
