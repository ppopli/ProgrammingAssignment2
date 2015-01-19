## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object 
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        
        inverse <- NULL  ## object to cache the inverse
        
        ## function to cache  the matrix 
        set <- function(y){
                x <<- y
                inverse <<- null
        }
        
        ## function returning the special matrix
        
        get <- function() x
        
        ## function to cache the inverse of the matrix
        
        setinverse <- function(m) inverse <<- m
        
        ##function to get inverse of the matrix
        
        getinverse <- function() m
        
        ## list is returned containing the functions to create special
        ## matrix and cache its inverse
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       
        m <- x$getinverse()
        ## condition to check if the inverse is cached 
        ## if it exists its value is returned
        
        if(!is.null(m)){
                message("getting cached inverse")
                return(m)
        }
        
        ## getting special matrix using get function to calculate 
        ## its inverse
        
        data <- x$get()
        
        ## using solve function to calculate its inverse
        ## and store it in variable m
        m <- solve(data)
        
        ## caching the inverse of the matrix using setinverse()
        
        x$setinverse(m)
        
        ## returning inverse of the matrix
        m
}
