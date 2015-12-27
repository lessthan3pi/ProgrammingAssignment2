## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    # assume 'x' is a square invertible matrix
    # return: a list containing fn to
    # 1. set matrix
    # 2. get matrix
    # 3. set inv
    # 4. get inv
    
    inv <- NULL
    set <- function(y){
        # <<- assign values to objects outside environment.
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
    
}


## checks cache to see if inv exists
# outputs inverse if so
# else, calculates inv, stores, and outputs inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinv()
    
    #check if inverse is caluated
    
    if(!is.null(inv)){
        
        message("getting cached data.")
        return(inv)
    }
    
    matrix <- x$get()
    
    inv <- solve(matrix, ...)
    
    #cache for future use
    x$setinv(inv)
    
    return(inv)
}
