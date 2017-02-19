## These two functions cache the inverse of a matrix and enable it to be 
## inserted into a function if it already exist in cache

## creates a cached matrix

makeCacheMatrix <- function(x = matrix()) {
    # set the value of the cache as NULL as nothing is cached yet
    inv <- NULL
    
    # function to set the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # function to return the matrix x
    get <- function() x
    # create the inverse of the matrix x as matinv
    matinv <- function(solve){
        inv <<- solve
    }
    # get the cached matrix
    getinv <- function() inv
    
    # list of functions
    list(set=set, get=get, matinv=matinv, getinv=getinv)
}


## creates an inverse of a matrix if not already in cache

cacheSolve <- function(x = matrix(), ...) {
     ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    
    # if a cached value exists, retrieve it
    if(!is.null(inv)) {
        message("Getting cached value")
        return(inv)
    }
    # if there's no cached value, we need to set it
    mat <- x$get()
    # get the inverse of the matrix
    inv <- solve(mat, ...)
    x$matinv(inv)
    # return result
    inv
}
