## Put comments here that give an overall description of what your
## functions do

##This function creates a list containing a function that sets the value of the matrix, gets the value of the matrix, 
## sets the value of the inverse of the matrix, and then gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set=set, get=get, setinverse=setinv, getinverse=getinv)
}


## This function assumes that any matrix you are testing is invertible.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
