## Put comments here that give an overall description of what your
## functions do

## This is a function of functions which creates a special "matrix" (from x). 
## The data that makes up the "matrix" can be stored such that its inverse 
## can be cached for late use (given x has the same data).

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() { x }
        setinverse <- function(inverse) { i <<- inverse }
        getinverse <- function() { i }
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function creates the inverse of x whilst storing that information for later use. 
## If x is called on again, and x has the same data, then it will return the stored inverse 
## matrix instead of calculating it again. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
