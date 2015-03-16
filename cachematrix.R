## makeCacheMatrix and cacheSolve functions are used to
## calculate and cache the inverse of a matrix
## 

## makeCacheMatrix takes a matrix as a formal argument and
## it returns a list of four functions used to calculate
## and cache the inverse of the matrix argument
## The functions contained in the result list are:
## set: Function to store the matrix in another workspace and to
##      set the inverse matrix cache to NULL
## get: Function to return the matrix argument
## setinverse: Function to calculate the inverse of matrix x
## getinverse: Function to return: 
##                the cached inverse of matrix x provided it has been computed
##                NULL if the inverse of matrix x has not been computed

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) im <<- solve
    getinverse <- function() im
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve takes the list of functions returned from makeCacheMatrix
## as a formal argument and it returns:
##    1. the cached inverse of matrix passed to makeCacheMatrix if it has
##       been calculated and cached
##    2. the newly calculated inverse of matrix passed to makeCacheMatrix
## If the inverse had not been previously calculated, it is cached for
## future use

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getinverse()
        if (!is.null(im)) {
            message("getting cached data")
            return(im)
        }
        data <- x$get()
        im <- solve(data,...)
        x$setinverse(im)
        im
}
