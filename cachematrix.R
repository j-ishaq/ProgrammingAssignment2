makeCacheMatrix <- function(x = matrix()) {
    ## Create a special "matrix" object that can cache its inverse
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


cacheSolve <- function(x, ...) {
    ## Compute the inverse of the special "matrix" returned by makeCacheMatrix() function
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
