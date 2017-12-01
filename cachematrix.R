## Computes the inverse of a matrix, assuming all input matrix are invertible

## stores the matrix and its inverse, also contains internal functions for
## storing and retrieving the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    cache<-NULL
    
    set<-function(a){
        x<<-a
        cache<<-NULL
    }
    get<-function() x
    setinverse<-function(inverse) cache<<-inverse
    getinverse<-function() cache
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## this fucntion accepts a matrix x and determines if an inverse is already
## computed, otherwise it computes it and save in the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv<-x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data<-x$get()
    inv<-solve(data)
    x$setinverse(inv)
    inv
}
