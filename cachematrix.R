## These two functions cache the inverse of a matrix.

##The first function, makeCacheMatrix creates a "special matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {                            ##matrix is set
                x <<- y
                m <<- NULL
        }
        get <- function() x				##this function gets back the matrix
        setinv <- function(solve) m <<- solve		##this function sets the inverse of the matrix
        getinv <- function() m				##this function gets back the inverse of the matrix
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The following function calculates the inverse of the "Special matrix". However, it first 
##checks to see if the matrix has already been inverted. If so, it gets the inverse matrix 
##from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix 
##and sets the inverse matrix in the cache via the setinv function.


cacheSolve <- function(x, ...) {    			## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {				## Checks is the matrix has already been inverted
                message("getting cached data")		## If so, gets back the inverse matrix from the cache
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
