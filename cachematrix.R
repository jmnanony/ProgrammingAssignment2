# A pair of functions used together that cache the inverse of a matrix.
# Create a special matrix by calling matrix <- makeCacheMatrix(x).
#   The matrix can be changed by using matrix$set(x)
# Get the inverse of the special matrix object by calling cacheSolve(matrix)
#   This will return a cached result if the inverse has already been solved.



# makeCacheMatrix() creates a special matrix object that can cache its inverse.
# This is really a list containing functions to:
#   set the value of the matrix
#   get the value of the matrix
#   set the value of the invers
#   get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# cacheSolve() solves the inverse of the special "matrix" created with makeCacheMatrix.
#   It first checks if the inverse has already been solved.
#   If so, it gets the inverse from the cache and skips the computation.
#   Otherwise, it solves the inverse of the matrix
#       and also sets the inverse in the cache with the setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
