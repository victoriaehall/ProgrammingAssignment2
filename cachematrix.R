## Below are two functions. The first caches a matrix and its inverse.
## The second functions retrieves the cached inverse matrix. If the
## inverse matrix has already been cached, that matrix is retrieved.

## Function 1
## This is the function that sets the matrix and its inverse.
## First, the function checks to make sure that agrument entered is a matrix;
## if it is not, an error message is produced.

makeCacheMatrix <- function(x = matrix()) {
        #test if x is matrix
        if (!is.matrix(x)) {
                message("Error: the agrument is not a matrix. try again")
        } else {
        i <- NULL
        set <- function(y) {
                if (!is.matrix(y)) {
                        message("Error: the agrument is not a matrix. try again")
                } else {
                x <<- y
                i <<- NULL
                }
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve(x)
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        }
}

## Function 2
## This function retrieves the cached inverse matrix that was set with the
## previous function. If an inverse matrix has already been cached and the
## matrix has not be altered, the cache is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
