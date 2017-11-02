## Function makeCacheMatrix: store the matrix to be inversed and provide
## a list of functions to get or set original matrix and inversed matrix
## 
## Function cacheSolve: receive an object of makeCacheMatrix, and calculate
## the inversed matrix or get the cached inversed matrix directly


## This function receive a matrix, initialize the internal variable im which
## is used to store the inversed matrix, return a list of 4 functions to get
## or set the original and inversed matrix.

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y){
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setInversed <- function(value) im <<- value
    getInversed <- function() im
    list(set = set, get = get,
         setInversed = setInversed, 
         getInversed = getInversed)
}


## This function receive an object of makeCacheMatrix, firstly try to obtain
## the inversed matrix from the object; if fail, calculate the inversed matrix
## by solve() function, and store the result into the makeCacheMatrix object
## for cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inversed_matrix <- x$getInversed()
    if(!is.null(inversed_matrix)) {
        message("getting cached inversed matrix")
        return(inversed_matrix)
    }
    data <- x$get()
    inversed_matrix <- solve(data)
    x$setInversed(inversed_matrix)
    inversed_matrix
}

## Example:
##
## > mc <- makeCacheMatrix(matrix(c(1,0,1,1,1,0,1,1,1),nrow = 3,ncol = 3))
## > mc$get()
## [,1] [,2] [,3]
## [1,]    1    1    1
## [2,]    0    1    1
## [3,]    1    0    1
## > cacheSolve(mc)
## [,1] [,2] [,3]
## [1,]    1   -1    0
## [2,]    1    0   -1
## [3,]   -1    1    1
## > cacheSolve(mc)
## getting cached inversed matrix
## [,1] [,2] [,3]
## [1,]    1   -1    0
## [2,]    1    0   -1
## [3,]   -1    1    1
