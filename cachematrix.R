## These functions work together to calculate the inverse of a matrix.
##  Inverting a matrix can be a costly computation so this will only do
##  a computation if one hasn't been performed previously.

## 1. Create a cacheMatrix object to store the matrix and its inverse
## 2. Pass the cacheMatrix object to cacheSolve to calculate the inverse

## Create a cacheMatrix object that can get/set a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
        xInverted <- NULL
        
        set <- function(y) {
                x <<- y
                xInverted <<- NULL
        }
        
        get <- function() x
        
        setInverse <- function(invertedMatrix) xInverted <<- invertedMatrix
        
        getInverse <- function() xInverted
        
        list(get = get, 
             set = set,
             getInverse = getInverse, 
             setInverse = setInverse)
}

## Returns the inverse of a cacheMatrix object
# This will return the inverse if it already exists
#   or calculate it if it doesn't
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invertedMatrix <- x$getInverse()
        
        # Return a previously calculated matrix
        if (!is.null(invertedMatrix)) {
                return(invertedMatrix)
        }
        
        # Get the matrix to calculate the inverse of
        originalMatrix <- x$get()
        
        # Invert the matrix
        invertedMatrix <- solve(originalMatrix)
        
        # Set the inverse of the cachMatrix object
        x$setInverse(invertedMatrix)
        
        # Return the inverted matrix
        invertedMatrix
}
