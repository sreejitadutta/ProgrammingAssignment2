## Functions to cache inverse of a matrix

## Constructs a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL

    ##set the matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ##get the matrix
    get <- function() {
    	m
    }

    ##set inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ##get inverse of the matrix
    getInverse <- function() {
        i
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
    m <- x$getInverse()

    ## Return the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get the matrix from our object
    data <- x$get()

    ## Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data

    ## Set the inverse to the object
    x$setInverse(m)

    ## Return the matrix
    m
}
