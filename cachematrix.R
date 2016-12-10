## Caching Matrix in  Inverse :
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly.
## Below are the  functions that  used to create a special object and  stores a matrix which caches its inverse.


##function to creates special "matrix" object that can cache  inverse.


makeCacheMatrix <- function(x = matrix()) {
        invM <- NULL
        set <- function(y) {
                x <<- y
                invM <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) invM  <<- inverse
        getInverse <- function() invM
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


##Function computes the inverse of the special "matrix" created by makeCacheMatrix 
##If the inverse has already been calculated then it should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invM <- x$getInverse()
        if (!is.null(invM)) {
                message("getting data from cache")
                return(invM)
        }
        matM <- x$get()
        invM <- solve(matM, ...)
        x$setInverse(invM)
        invM
}