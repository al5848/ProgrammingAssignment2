## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#' This function creates a special matrix object that can cache its inverse.
#' It returns a list containing a function to:
#' 
#' 1. set the value of the matrix
#' 2. get the value of the matrix
#' 3. set the value of the inverse matrix
#' 4. get the value of the inverse matrix
#' 
#' @param x matrix to inverse
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

#' This function computes the inverse of the special matrix object returned by makeCacheMatrix.
#' It returns a the inverse matrix of the special matrix 'x'.
#' 
#' @param x special matrix returned from makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
