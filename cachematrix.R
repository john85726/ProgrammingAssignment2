## The overall purpose of these functions is to calculate
## the inverse of a given matrix. The matrix and its inverse
## matrix are stored in cache. So when the given matrix has
## already been calculated and stored in cache, the inverse
## will be recalled from the cache and skip the calculation.


## This function creates a special "matrix" object that can
## cache its inverse. It contains a list of functions:
## setmatrix - set the value of a matrix
## getmatrix - get the value of a matrix
## setinverse - set the cached inverse matrix value
## getinverse - get the cached inverse matrix value

makeCacheMatrix <- function(x = matrix()) {
        # Make sure initially nothing is cached
        m <- NULL
        # Store a matrix
        setmatrix <- function(y){
                x <<- y
                # Replace whatever in cache with a new value
                m <<- NULL
        }
        # Return the cached matrix
        getmatrix <- function(){
                x
        }
        # Set the cached inverse matrix value
        setinverse <- function(solve){
                m <<- solve
        }
        # Get the cached inverse matrix value
        getinverse <- function(){
                m
        }
        # Return a list of functions
        list(setmatrix = setmatrix,
             getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the 
## inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        # If the inverse matrix is already cached,
        # return the cached value
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        # Otherwise, get the matrix and cached its
        # inverse matrix
        data <- x$getmatrix()
        m <- solve(data)
        x$setinverse(m)
        # Return the inverse matrix
        m
}
