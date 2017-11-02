## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
