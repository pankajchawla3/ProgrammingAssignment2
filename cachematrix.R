## makeCacheMatrix() returns a list of functions used to cache a matrix and its
## inverse. 
## cacheSolve() is used to compute/retrieve the inverse matrix.


## Returns a list of these 4 functions-
## 1) get() returns the current matrix
## 2) set() caches a new martix and resets the inverse matrix to NULL.
## 3) setinverse() caches the inverse matrix.
## 4) getinverse() returns the inverse of the current matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setinverse <- function(i) inv <<- i
    getinverse <- function() inv
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Retrieves the cached inverse matrix, if available, else computes and caches it.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    ## second argument to solve() is the identity matrix.
    inv <- solve(data, diag(nrow(data)), ...)
    x$setinverse(inv)
    inv
}
