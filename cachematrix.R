## The following two functions are used to cache an invertible
## matrix into an object (makeCacheMatrix) and to calculate the
## inverse of the cached matrix, if not previously calculated
## (cacheSolve).

## The makeCacheMatrix function takes an invertible
## matrix and creates a new Matrix object. To use 
## this function, initialize it with one of the following
## approaches:
##   x <- makeCacheMatrix( myMatrix )   or
##   x <- makeCacheMatrix()
##   x$set( myMatrix )
##
## Call x$setInv() to create the inverse matrix, then use
## x$get() or x$getInv() to get the original matrix or its
## inverse, respectively.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInv <- function(solve) i <<- solve
    getInv <- function() i
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## The cacheSolve function will take a makeCacheMatrix object
## and return the inverse of the matrix in the object. If the
## inverse has previously been calculated the cached version is
## returned, otherwise the inverse is calculated and cached in
## the object and then returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInv(i)
    i
    
}
