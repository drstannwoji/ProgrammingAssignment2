## Programming Assignment 2: Lexical Scoping
## The two functions are to cache the inverse of a Matrix and to compute the 
inverse of the special Matrix returned by the MakeCacheMatrix.

## The MakeCacheMatrix is a function that creates a special matrix object that can cache the inverse.
The CacheSolve function computes the inverse of the special Matrix returned by the MakeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}

## The function above creates a special matrix object that can cache the inverse.

## The CacheSolve function computes the inverse of the special Matrix returned by the MakeCacheMatrix.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached matrix")
                return (inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
