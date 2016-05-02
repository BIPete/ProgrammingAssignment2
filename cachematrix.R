## cachematrix.R - Cache the inverse of a matrix
## This pair of functions can be used to solve the inverse of large matrix
## objects by using lexical scoping
##
## NB: These functions are used in conjunction with one another
##
## Sample usage:
## >y <- matrix(c(4,7,2,6),nrow=2,ncol=2,byrow=TRUE)
## >matrixinv <- cacheSolve(makeCacheMatrix(y))

makeCacheMatrix <- function(x = matrix()) {
        # This function takes a matrix and creates a special "vector", which
        # is a list of 4 functions that can then be used by the cacheSolve
        # function to build the inverse of the original matrix
        # 
        # Args: 
        #       x: an invertable matrix
        #
        # Returns: 
        #       A list consisting of the 4 functions defined
        
        inv <- NULL ## set inv as NULL
        
        set <- function(y) {
                # Set the value of the matrix in another environment
                x <<- y ## Assign x in another environment 
                inv <<- NULL ## Assign inv in another environment
        }
        get <- function() x
        # Get the value of the matrix
        setinverse <- function(inverse) inv <<- inverse
        # Set the value of the inverse matrix
        getinverse <- function() inv
        # Get the value of the inverse matrix
        
        # Return list of the 4 functions
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
        # This function creates the inverse matrix of the matrix used as the
        # argument in the makeCacheMatrix function
        # 
        # Args:
        #       x: a list of 4 functions created by the makeCacheMatrix function
        #
        # Returns:
        #       the inverse matrix
        
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        
        # Return inverse matrix
        inv
}
