## The below functions will set an inversible matrix and calculate its 
## inverse
## In case the matrix has not changed, thus the calculation of its inverse 
## does not need to be calculated again, the cached inverse matrix will
## returned. 

## The below function will create four functions that allow the creation 
## of a matrix and the storing of its inverse. The functions are:
## - set: resets the original matrix. Every time the matrix is set, m is 
## set to NULL
## - get: returns the original matrix
## - cacheinv: caches the inverse matrix. Every time the inverse is 
## calculated, m is set to the inverse.
## - retinv: returns the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
          x <<- y
          m <<- NULL
        }
        get <- function() x
        cacheinv <- function(inv) m <<- inv
        retinv <- function() m
        list(set = set, get = get,
            cacheinv = cacheinv,
            retinv = retinv)
}


## The below function gets a matrix as an argument and checks whether its
## inverse has been calculated. If it has been calculated, the cached
## inverse matrix is being returned, otherwise it's being calculated and
## cached for future use.

cacheSolve <- function(x, ...) {
        m <- x$retinv()
        if(!is.null(m)) {
          message("Please wait till cached matrix is retrieved...")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$cacheinv(m)
        m
}
