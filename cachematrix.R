## Compute an inverse matrix, allowing it to be stored in the cache
# and recalled if called again

## Create a special vector to store the inverse of a matrix in the cache

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list ( set = set, get = get, 
               setinv = setinv,
               getinv = getinv)
}


## Find and return inverse matrix if in the cache.  If not, compute the inverse

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}