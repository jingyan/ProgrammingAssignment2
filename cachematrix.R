## Below two functions are used to create a special object which stores a matrix and 
## cache its inverse.

## This function is used to create a specia "maxtrix" object, which is really a list of 
## functions. These functions could be used to get and set the matrix as well
## as its inverse.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i<<-inverse
        getinverse <- function() i
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## This function would calculate the inverse of the special "matrix" object. It will 
## return inverse from cache if the cache is available. Otherwise, it will solve it,
## put the result into cache, and return the result.
cacheSolve <- function(x, ...) { 
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
