## This function creates a special "matrix" object that can cache its inverse.
## matrix are always inversible

## this function set up matrix for inverse calulcation
## set the value of the Matrix
## get the value of the Matrix
## set the value of the Inverse
## get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {

 m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
}

 
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}



## this fuction return inverse of matrix - if matrix is already in cache function does not calculate again

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
## function return m which is inverse of the matrix for cache - exiting matrix
        }
        data <- x$get()
        m <- solve(data, ...)
 ## Solve function is used to compute inverse.
        x$setinv(m)
        m
## function return m which is inverse of the matrix for new inverse calculation
}
