## Matrix inversion is usually a costly computation and there is 
## (some) benefit to caching the inverse of a matrix rather than 
## compute it repeatedly. The functions mentioned in the file 
## caches the inverse of a matrix.



## The makeCacheMatrix function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set  <- function(y) { 
                x <<- y
                M <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, 
             get = get, 
             setmean = setmean, 
             getmean = getmean)
}


## The cacheSolve function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse 
## has already been calculated ( and the matrix has nog changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getmean()
        if (is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}
