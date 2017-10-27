## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## TODO: Add type checking to ensure that x is a matrix
    
    ## Initialize the variable holding the inverse matrix to NULL
    inverse_x = NULL
    
    ## Allow a new state to be set for encapsulated matrix, and set
    ##     the value of the variable holding the inverse matrix to NULL
    set <- function(y = matrix()) {
        ## TODO: Add type checking to ensure that x is a matrix
        x <<- y
        inverse_x <<- NULL
    }
    
    ## Retrieve and return the current internal encapsulated matrix
    get <- function() x
    
    ## Set the value of internal variable pointing to the inverse
    ##     of the matrix 
    setinverse <- function(i) inverse_x <<- i
    
    ## Retrieve and return the current internal encapsulated 
    ##     inverse matrix
    getinverse <- function() inverse_x
    
    ## Return a list of object state when makeCacheMatrix is called
    list(set=set, get=get, 
         setinverse=setinverse, 
         getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## TODO: Check that x is a makeCacheMatrix matrix
    ## TODO: Check that x$get() is a matrix
    ## TODO: Check that x$get() is an invertible matrix
    ##     If det(a_matrix) != 0 then a_matrix is an
    ##     invertible matrix

    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    
    ## Check to see if the cached version of the inverse
    ##     matrix was retrieved from the matrix passed into 
    ##     this function. If so, return it.
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## Calculate the inverse of the matrix passed into
    ##     the function
    i <- solve(x$get())
    
    ## Set the inverse of the matrix in the special matrix object
    x$setinverse(i)
    
    ## Return the inverse matrix
    i
}
