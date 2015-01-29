## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

    inverse <- NULL
    set <- function (y) {
        x <<- y
        inverse <<- NULL
    }

    get <- function () {
        x
    }
    
    setInverse <- function (passedInverse) {
        inverse <<- passedInverse
    }

    getInverse <- function () {
        inverse
    }

    list(
        set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse 
    )
}


## Write a short comment describing this function
# this function will handle the cacheMatrix object, and either
# retrieve a cached inverse for the current matrix, or create
# a new inverse and store it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()

    # there's already a cached inverse
    if(!is.null(inverse)) {
        message('getting cached data')
        return(inverse)
    }

    # if not, get the original value of x, invert it and store it
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
    
}
