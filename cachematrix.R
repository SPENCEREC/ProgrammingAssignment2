## Creates a matrix object and caches its value
## and the value of its inverse, retrieves values.

## Creates list of functions to use when caching matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        seti <- function(inverse) i <<- inverse
        geti <- function() i
        list(set=set,get=get,seti=seti,geti=geti)
}


## Returns matrix inverse of "x" by looking if one already exists,
## showing it if so, and making it if not.

cacheSolve <- function(x, ...) {
        i <- x$geti()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data,...)
        x$seti(i)
        i
}
