## This is pair of functions that in repsonce to Coursera R Programming Course
## 

## makeCacheMatrix sets a matrix, gets a matrix, calculates the matrix inverse, and returns the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
         x <<- y
         m <<- NULL
    }
    get <- function() x
    setInv <- function(solve) m <<- solve
    getInv <- function() m
    list(set = set, 
         get = get,
         setInv = setInv,
         getInv = getInv)
}

## Write a short comment describing this function
## cacheSolve function return the inverse of a matrix, either cached or calculated

cacheSolve <- function(x, ...) {
    m <- x$getInv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
    m
}
