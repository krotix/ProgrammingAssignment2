## Create an object that can cache matrix inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    ## Set the matrix
    set <- function( matrix ) {
        m <<- matrix
        inv <<- NULL
    }

    ## Get the matrix
    get <- function() {
        m
    }

    ## Set the inverse of the matrix
    setInverse <- function(inverse) {
        inv <<- inverse
    }

    ## Get the inverse of the matrix
    getInverse <- function() {
        inv
    }

    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    ## Return the inverse matrix if its already cached
    if( !is.null(m) ) {
        message("getting cached data")
        return(m)
    }

    data <- x$get()

    ## Calculate the inverse matrix
    m <- solve(data) %*% data

    x$setInverse(m)

    ## Return the matrix
    m
}
