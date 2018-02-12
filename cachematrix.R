## The pair of functions cache the inverse of a matrix

## makeCacheMatrix creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## computes the inverse of the special matrix returned by makeCacheMatrix 
## if the mean has already been calculated it's got from the cache
## otherwise calculates it and sets it in the cache

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
