## Put comments here that give an overall description of what your
## functions do

# The purpose of this assignment is to write an R function that is
# able to cache computations for finding the inverse of a matrix.
# If a matrix is very large it will take a long time to compute
# its inverse, especially if it has to be computed repeatedly.
# To make the computation more efficient, is is best to cache
# the computed inverse especially for contents of matrices that
# are not changing. To achieve this, two functions  have been
# created:
# 1.  makeCacheMatrix caches the inverse of a matrix and returns
#                     a list of functions to manage caching.
# 2.  cacheSolve computes the inverse of the special matrix object
#                returned from makeCacheMatrix.   

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
