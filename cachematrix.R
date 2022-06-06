## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    nv <- NULL
    set <- function(y) {
        x <<- y
        nv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) nv <<- inverse
    getinverse <- function() nv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function----
##function calculates the inverse of the special “matrix” created by makeCacheMatrix

cacheinverse <- function(x, ...) {
    nv <- x$getinverse()
    if(!is.null(nv)) {
        message("getting cached data")
        return (nv)
    }
    matrix_to_invert <- x$get()
    nv <- solve(matrix_to_invert, ...)
    x$setinverse(nv)
    nv
}
