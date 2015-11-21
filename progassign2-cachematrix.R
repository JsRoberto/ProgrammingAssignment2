## The following functions cache an invertible matrix and its inverse 
## and show the inverse itself.

## This function caches an invertible matrix (including possibly the inverse
## itself) in a list containing functions which set and get the data.

makeCacheMatrix <- function(x = matrix()){
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)
}


## This function interact dinamicaly with the above function in order to set
## and get the inverse, testing whether the inverse was obtained preveously
## and, if not, setting it correctly before to get this result.

cacheSolve <- function(x, ...){
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
