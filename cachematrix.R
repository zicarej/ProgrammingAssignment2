## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes a square, invertible matrix as an argument and returns a
## list of four functions.
##
## For example, x<-makeCacheMatrix() returns:
##    x$get(x) returns x
##    x$getinverse(x) returns x inverse
##    x$set(x) archives x
##    x$setinverse(x) archives x inverse
##

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get
        m <- solve(data, ...)
        x$setinverse(m)
}
