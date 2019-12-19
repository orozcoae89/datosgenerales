## This script is intended to cache the inverse of a matrix

## This function creates a special "matrix" object that can cache 
## its inverse

makeCacheMatrix <- function(x = matrix()) {
        matrix.inverse <- NULL
        set <- function(y) {
                x <<- y
                matrix.inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) matrix.inverse <<- solve
        getinverse <- function() matrix.inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrix.inverse <- x$getinverse()
        if(!is.null(matrix.inverse)) {
                message("getting cached matrix inverse")
                return(matrix.inverse)
        }
        data <- x$get()
        matrix.inverse <- solve(data, ...)
        x$setinverse(matrix.inverse)
        matrix.inverse
}
