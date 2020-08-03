##function makeCacheMatrix:
##1. set the value of the matrix
##2. get the value of the matrix
##3. set the value of the inverse
##4. get the value of the inverse
## 
##: cacheSolve:
##Computes the inverse of the special "matrix" returned 
##by makeCacheMatrix aboveT. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve the 
##inverse from the cache.

## setting and getting the special matrix

makeCacheMatrix <- function(x = matrix()) {
        invrs <- NULL
        set <- function(y) {
                x <<- y
                invrs <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) invrs <<- solve
        getsolve <- function() invrs
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        invrs <- x$getsolve()
        if(!is.null(invrs)) {
                message("getting cached data for inverse matrix")
                return(invrs)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(invrs)
        invrs           
}