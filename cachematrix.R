## These two functions offer the ability to caculate, cache and retrieve the inverse of
## a matrix.

## makeCacheMatrix allows the caller to store and retrieve a cached, inverted, matrix.
## Embedded functions:
##      set: This function will reset the makeCacheMatrix object by setting the cache to NULL
##           and setting the un-inverted matrix to that passed.
##      get: This function will retrieve the matrix used to construct the makeCacheMatrix object.
##      setInverse: This function will set the globally accesible vector, acting as the cache
##                  to the the inversed matrix passed.
##      getInverse: This function will attempt to retrieve the cached inverse of the matrix.
## The embedded functions are exposed to the caller as a list of function objects.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y) {
        inverse <- NULL
        x <<- y
    }
    
    get <- function() {
        x
    }
    
    setInverse <- function(i){
        inverse <<- i
    }
    
    getInverse <- function(){
        inverse
    }
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve function will accept a makeCacheMatrix object and attempt to retrieved a cached
## inverted matrix from it. If a cached value cannot be retrieved the function will get the 
## un-inverted matrix from the object (used to construct it), invert the object and set the 
## result to the cache.

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    
    if(!is.null(inverse))
    {
        return(inverse)
    }
    else
    {
        mat <- x$get()
        
        x$setInverse(try(solve(mat)))
        
        return(x$getInverse())
    }
}
