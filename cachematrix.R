## BOF ##

## Briefing: Caching the inverse of a Matrix object ##

# This function creates a special "matrix" object that can cache its inverse.
# Params: x -> this is a matrix object
# Return: The matrix object
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse 
        getinverse <- function() i 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cacheSolve should retrieve the inverse from the cache.
# Params: x   -> this is a matrix object created by the makeCacheMatrix() function
#         ... -> any other args. that could be used by the solve() R function
# Return: a matrix that is the inverse of "x"
cacheSolve <- function(x, ...) {
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

## EOF ##