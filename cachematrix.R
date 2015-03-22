## makeCacheMatrix creates a list containing the following function
##set the value of the vector
##get the value of the vector
##set the value of the mean
##get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL
    set <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invMatrix <<- inverse
    getinverse <- function() invMatrix
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    invMatrix <- x$getinverse()
    if(!is.null(invMatrix)) {
        message("Fetching from cached data")
        return(invMatrix)
    }
    data <- x$get()
    invMatrix <- solve(data)
    x$setinverse(invMatrix)
    invMatrix
}
