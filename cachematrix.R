## Put comments here that give an overall description of what your functions do

## This function creates a special "matrix" object that can cache its inverse
## it will create a list containing the original matrix and the inverse of if

## main goal is to avoid costly resolving for a matrix
## sample invertible matrix a <- matrix(c(1,-1,2,10,-1,3,-3,-4,3), nrow = 3, ncol = 3,)

## First Function to return a list with data and the inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## set function
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## get function 
        get <- function() x
        ## set inverse
        set_inverse <- function(solve) m <<- solve
        ## get inverse
        get_inverse <- function() m
        ## return list
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

## The second function is going to utilize the functions from above to perform the 
## cache checking and calculation in the event that the cache is not present. 

cacheSolve <- function(x, ...) {
        m <- x$get_inverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_inverse(m)
        m
}





