## These two functions cache the inverse of a matrix.

##The first function, makeCacheMatrix creates a "special matrix" object that can cache its inverse
##It is actually a list containing a function to

    ##set the matrix 
    ##get the matrix
    ##set the inverse of the matrix
    ##get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {	
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The following function calculates the inverse of the "Special matrix". However, it first 
##checks to see if the matrix has already been inverted. If so, it gets the inverse matrix 
##from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix 
##and sets the inverse matrix in the cache via the setinv function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
