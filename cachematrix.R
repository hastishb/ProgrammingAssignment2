## Put comments here that give an overall description of what your
## functions do
## There are 2 functions: 
## 1- makeCacheMatrix: this function creates a list 
##    that each of its elements are functions
## 2- cacheSolve: this function gets a list created 
##    with the makeCacheMatrix function and returns 
##    the inverse of the matrix 

## Write a short comment describing this function
## Four functions are defined in this function and the are added to a list;
## using these functions we can set or get a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        setmatrix <- function(y){
                x <<- y
                i <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(setmatrix = setmatrix, getmatrix = getmatrix, 
             setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## In this function, a list created with the makeCacheMatrix function is passed in
## as an argument. Then it is checked if the getinverse function retruns anything
## If there's aleardy an inverse cached in, it is returned, otherwise the inverse is 
## calculated, set(cached), and then returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        m <- x$getmatrix()
        inverse <- solve(m)
        x$setinverse(inverse)
        inverse
}
