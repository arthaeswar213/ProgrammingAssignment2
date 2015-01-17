## This R file is to get a inverse of a squared matrix either from cache or 
## by computing


## This function makeCacheMatrix is to store the inverse matrix in cache 
##  and retrieve it when necessary

makeCacheMatrix <- function(x = matrix()) {
    mat_inv <- NULL
    set <- function(y) {
        x <<- y
        mat_inv <<- NULL
    }
    get <- function() x
    setinvmat <- function(solve) mat_inv <<- solve
    getinvat <- function() mat_inv
    list(set = set, get = get,
         setinvmat = setinvmat,
         getinvmat = setinvmat)

}


## Write a short comment describing this function

## This function is to get the inverse of a matrix either from cache or 
## compute it and then call the function to store it in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    mat_inv <- x$getinvmat()
    if(!is.null(mat_inv)) {
        message("getting cached data")
        return(mat_inv)
    }
    data <- x$get()
    mat_inv <- solve(data,...)
    x$setinvmat(mat_inv)
    mat_inv
}
