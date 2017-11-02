## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInversed <- function(value) i <<- value
    getInversed <- function() i
    list(set = set, get = get,
         setInversed = setInversed, 
         getInversed = getInversed)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInversed()
    if(!is.null(i)) {
        message("getting cached inversed matrix")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setInversed(i)
    i
}
