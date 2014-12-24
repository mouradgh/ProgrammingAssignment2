## Coursera, R Programming, Johns Hopkins University
## Assignment 2 : Caching the Inverse of a Matrix


#creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function( x = matrix() ) {
    
    #Initializing the inverse
    m <- NULL
    
    #Matrix Setter
    set <- function( matrix ) {
        x <<- matrix
        m <<- NULL
    }
    
    #Matrix getter
    get <- function() x
    
    #Set the inverse of the matrix
    setInverse <- function(inverse) m <<- inverse
    
    #Get the inverse of the matrix
    getInverse <- function() m
    
    #List of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


# Computes the inverse of the special "matrix" returned by makeCacheMatrix 
# Should retrieve the inverse from the cache if the inverse has 
## already been calculated 
cacheSolve <- function(x, ...) {
    #Get the inverse of the matrix
    m <- x$getInverse()
    
    #Return the inverse if its already calculated
    if(!is.null(m) ) {
        message("getting cached data")
        return(m)
    }
    
    #Get the matrix
    data <- x$get()
    
    #Calculate the inverse
    m <- solve(data, ...)
    
    #Set the inverse
    x$setInverse(m)
    
    #Return the matrix
    m
}
