## This is implementation of special matrix - makeCacheMatrix, 
## which can cache value of the inverse matrix
## Important: x is assumed that the matrix supplied is always invertible

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        
        ## inv - variable that stores inverse matrix
        inv <- NULL
        
        ## function sets new value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## function returns matrix
        get <- function() x
        
        ## function sets new value of inv variable 
        setinv <- function(p_inv) inv <<- p_inv
        
        ## function returns inverse matrix
        getinv <- function() inv
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        
        ## If inverse of matrix x has alredy calculated, return inverse matrix
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## else calculate inverse matrix and cache it
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        
        inv
}
