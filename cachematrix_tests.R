## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## points to achieve for assignment
## 1 - Valid GitHub Repository valid
## 2 - At least one commit performed
## 3 - SHA1 submitted? shows commit
## 4 - SHA1 pointint to a special commit?

## 5 - Check if (any) code as been submited no judging if correct or not
## 6 - Comments explaining the code?
## 7 - makeCacheMatrix function correct?
## 8 - cacheSolve correct?

## This function creates a special "matrix" object that can cache its inverse
## it will create a list containing the original matrix and the inverse of if

## main goal is to avoid costly resolving for a matrix
a <- matrix(c(1,-1,2,10,-1,3,-3,-4,3), nrow = 3, ncol = 3,)

simple_solve <- function(x = matrix()) {
        ## 1 - get matrix
        orig_mat <- x
        ## set function
        inv_mat <- solve(x)
        ## return inverse
        inv_mat
}


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## set function
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## get function 
        get <- function() x
        ## setmean 
        set_inverse <- function(solve) m <<- solve
        ## getmean 
        get_inverse <- function() m
        ## X - return list
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

## The second function is going to utilize these functions to perform the 
## cache checking and calculation in the event that the cache is not present. 
## As in the example this function will not be recursive.

cacheSolve <- function(x, ...) {
        m <- x$get_inverse()
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_inverse(m)
        m
}





