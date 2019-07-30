## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# A special "matrix" is created by this function makeCacheMatrix, 
# which is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function
# cacheSolve function calculates the inverse of the special "matrix" 
# created with the makeCacheMatrix function. 
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. Otherwise, 
# it calculates the inverse of the data and sets the value of the inverse in the 
# cache via the setmean function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setsolve(m)
        m
}
