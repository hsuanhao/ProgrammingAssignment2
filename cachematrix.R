## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there
## may be some benefit to caching the inverse of a matrix rather
## than compute it repeatedly.

## makeCacheMatrix is the function to create a special "matrix"
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get=get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve function is to compute the inverse of the special
## "matrix" returned by makeCacheMatrix function. If the inverse
## has already been calculated, then the cacheSolve retrieves the
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinv(inv)
        inv
}
