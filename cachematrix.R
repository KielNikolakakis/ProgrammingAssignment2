## Kiel Nikolakakis
## R Programming - Assignment 2
## 2016-01-20
## 
## These functions are to calculate and cache the inversion of a square matrix,
## and retain the inversion in cached memory. If already calculated, the cached
## inversion will be returned, and the calculation skipped.

## makeCacheMatrix() sets up a cachable "matrix" object

makeCacheMatrix <- function(x = matrix()) {
    cache = NULL
    
    get <- function() x
    setinverse <- function(inverse) cache <<- inverse
    getinverse <- function() cache
    
    list(get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve() solves for the inverse of the matrix originally created with
## makeCacheMatrix() and returns the calculated inverse. However, if the inverse
## was previously calculated, the cached matrix is returned and the calculation
## skipped.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    I <- x$getinverse()
    
    if(!is.null(I)) {
        message("Retrieving cached matrix")
        return(I)
    }
    
    m <- x$get()
    I <- solve(m)
    x$setinverse(I)
    I
}
