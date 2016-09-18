## Functions to cache inverse of a matrix

## Constructs a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    ##set the matrix
    set <- function( matrix ) {
            m <<- matrix
            inv <<- NULL
    }

    ##get the matrix
    get <- function() {
    	m
    }

    ##set inverse of the matrix
    setInverse <- function(inverse) {
        inv <<- inverse
    }

    ##get inverse of the matrix
    getInverse <- function() {
        inv
    }

    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## Calculates the inverse of the matrix returned by "makeCacheMatrix".
## If the inverse for the same matrix has already been computed, 
## then the "cacheSolve" should return the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()

    ## Return the inverse if its already set
    if( !is.null(inv) ) {
            message("getting cached data")
            return(inv)
    }

    ## Get the matrix
    matr <- x$get()

    ## Calculate the inverse using matrix multiplication
    inv <- solve(matr, ...)

    ## Set the inverse
    x$setInverse(inv)

    inv
}
