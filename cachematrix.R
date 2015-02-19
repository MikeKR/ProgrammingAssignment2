## The following functions: 
## 1) cache a matrix. 
## 2) calculate its inverse and add it to the cache. 
## This allows other programs to retrieve the matrix
## and its inverse, as needed.


## makeCacheMatrix provides an initial value for the matrix
## x (which is then stored as a local variable), and returns a list 
## of access functions:
##   set -- changes the value of the matrix.
##   get -- retrieves the cached matrix.
##   setinverse -- stores the matrix inverse.
##   getinverse -- retrieves the inverse from the cache.
##   Example usage:
##   cacheObject <- makeCacheMatrix(matrix(data=c(1,2,3,4), nrow=2, ncol=2))

makeCacheMatrix <- function(x = matrix()) {

        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverseIn) inverse <<- inverseIn
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The argument to cacheSolve is a cache object, x.
## cacheSolve returns the matrix inverse, if it is in the
## cache object; otherwise, cacheSolve computes the matrix  
## inverse, stores it in x, and then returns it.
## Example usage:
## matrixInverse <- cacheSolve(cacheObject)

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                # The "message" is sent to stderr,
                # and is not visible in the RStudio
                # console by default.
                return(inverse)
        }
        data <- x$get()
        ## Note: It is assumed that the matrix in the 
        ## cache object is invertible.  If not, solve()
        ## will return an error (system is singular).
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
