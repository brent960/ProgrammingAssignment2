## These two functions, makeCacheMatrix and cacheSolve, work together to implement
##     a means to cache the inverse of a matrix alongside it.
##     The makeCacheMatrix function returns a list consisting of four functions
##     that are used to manage the state of an encapsulated matrix and its
##     inverse. The cacheSolve function actually performs the calculation of the
##     the inverse of the encapsulated matrix and stores the inverse in the 
##     makeCacheMatrix matrix.

## To create an invertible matrix to test these functions, use the following R code:
## m <- matrix( c(5, 1, 0,
##                3, -1, 2,
##                4, 0, -1), nrow=3, byrow=TRUE)

## Exercise the functions like this:
## my_cache_matrix <- makeCacheMatrix(m)
## cache_solve(my_cache_matrix)

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
##     This function takes as its single argument an invertible matrix.
##     This function returns a list consisting of two getter functions and two setter functions.
##     One getter/setter pair store and retrieve an invertible matrix, respectively.
##     A second getter/setter pair store and retrieve the inverse of the matrix.

## The implementation of this function includes testing of the arguments
##     to the makeCacheMatrix() function and the makeCacheMatrix$set() function
##     to ensure that they are invertible matrices.

makeCacheMatrix <- function(x = matrix()) {
    ## Add argument checking to ensure that x is a matrix
    if (class(x) != "matrix") {
        stop("Error in calling makeCacheMatrix: the argument to the function was not a matrix.")
    }
    
    ## Add argument checking to ensure that x is an invertible matrix
    ##     See this resource for a definition of invertible matrices:
    ##     https://cran.r-project.org/web/packages/matlib/vignettes/inv-ex1.html
    if (det(x) == 0) {
        stop("Error in calling makeCacheMatrix: the argument to the function was not an invertible matrix.") 
    }
    
    ## Initialize the variable holding the inverse matrix to NULL
    inverse_x = NULL
    
    ## Allow a new state to be set for encapsulated matrix, and set
    ##     the value of the variable holding the inverse matrix to NULL
    set <- function(y = matrix()) {
        ## Add type checking to ensure that y is a matrix
        if (class(y) != "matrix") {
            stop("Error in calling makeCacheMatrix$set: the argument to the function was not a matrix.")
        }
        ## Add argument checking to ensure that y is an invertible matrix
        ##     See this resource for a definition of invertible matrices:
        ##     https://cran.r-project.org/web/packages/matlib/vignettes/inv-ex1.html
        if (det(y) == 0) {
            stop("Error in calling makeCacheMatrix$set: the argument to the function was not an invertible matrix.") 
        }
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
    
    ## Return a list of the four getter/setting functions when
    ##     makeCacheMatrix is called
    list(set=set, get=get, 
         setinverse=setinverse, 
         getinverse=getinverse)
}


## The cacheSolve function computes the inverse of the special "matrix" 
##     returned by makeCacheMatrix above. If the inverse has already been 
##     calculated (and the matrix has not changed), then cacheSolve should 
##     retrieve the inverse from the cache. This function takes as its
##     single argument a makeCacheMatrix "matrix," and it returns the 
##     inverse of the matrix encapsulated within the makeCacheMatrix matrix.
##     If a cached version of the inverse matrix can be found, then that is printed
##     before the inverse matrix is returned. If a cached version cannot be found in the
##     makeCacheMatrix matrix, then the inverse is newly calculated, stored
##     in the instance of the makeCacheMatrix matrix, and returned.

## The implementation of this function includes some tests that the type
##     of the argument is valid, to ensure that a makeCacheMatrix matrix is
##     passed into the cacheSolve function, and that the matrix encapsulated
##     by the makeCacheMatrix matrix is invertible.

cacheSolve <- function(x, ...) {
    ## Check that x is a list like the makeCacheMatrix matrix
    if (class(x) != "list") {
       stop("Error in calling cacheSolve: the argument to the function was not a matrix created with makeCacheMatrix.") 
    }
    
    ## Check that the list contains all of the functions of the makeCacheMatrix list type
    if (is.null(x[["get"]]) || is.null(x[["set"]]) || is.null(x[["setinverse"]]) || is.null(x[["getinverse"]])) {
        stop("Error in calling cacheSolve: the argument to the function was not a matrix created with makeCacheMatrix.")
    }
    
    ## Add type checking to ensure that x$get() returns a matrix
    if (class(x$get()) != "matrix") {
        stop("Error in calling cacheSolve: the argument to the function was not a valid matrix created with makeCacheMatrix.")
    }
    
    ## Check that x$get() is an invertible matrix.
    ##     See this resource for a definition of invertible matrices:
    ##     https://cran.r-project.org/web/packages/matlib/vignettes/inv-ex1.html
    if (det(x$get()) == 0) {
        stop("Error in calling cacheSolve: the argument to the function was not an invertible makeCacheMatrix matrix.") 
    }

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
