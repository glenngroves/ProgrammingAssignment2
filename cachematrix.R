## To describe makeCacheMatrix using two different terminologies - object oriented, and R:

## In object oriented terminology, makeCacheMatrix:
##      Receives a matrix of data
##      Insubstantiates an object
##      Creates four public methods
##      The four public methods allow access to and updating of private attributes of the object called x and m 

## In R terminology, makeCacheMatrix:
##      Receives a matrix of data
##      Creates an object
##      Creates four functions returned in a list (therefore providing public access to those functions)
##      The four functions allow access to and updating of variables stored within the parent environment called x and m

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinversematrix <- function(inversematrix) m <<- inversematrix
    getinversematrix <- function() m
    list(set = set, get = get,
         setinversematrix = setinversematrix,
         getinversematrix = getinversematrix)
}

## cacheSolve:
## 1. Receives a matrix
## 2. Check if that matrix has been inverted and stored previously; if it was, it returns that previously inverted matrix
## 3. If the matrix was not inverted previously, it gets the source data, inverts it, stores the inverted data, and returns the inverted data

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinversematrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinversematrix(m)
    m
}

## examples
## m <- matrix(c(1,56,4,5656,5,3434,6,34343,54),nrow=3,ncol=3)
## mcm1 <- makeCacheMatrix(m)
## cacheSolve(mcm1)

## m <- matrix(rnorm(9),nrow=3,ncol=3)
## mcm2 <- makeCacheMatrix(m)
## cacheSolve(mcm2)
