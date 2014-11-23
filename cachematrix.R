## matrix object caching its inverse.
makeCacheMatrix <- function(x = matrix())) {
        matInv <- NULL
        set <- function(y) {
                x <<- y
                matInv <<- NULL
        }
        ## get matrix value
        get <- function() x
        
        ## set matrix inverse
        setInv <- function(i) matInv <<- i
        getInv <- function() matInv
        
        ## get matrix inverse
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

## If inverse was calculated, then retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        ## get matrix inverse
        matInv <- x$getInv()
        
        ## if theres a matrix print a message.
        if(!is.null(matInv)) {
                print("getting cached data")
                return(matInv)
        }
        
        ## if there isn't a matrix, get the inverse
        data <- x$get()
        matInv <- solve(data, ...)
        
        ## set matrix inverse 
        x$setInv(matInv)
        matInv
}