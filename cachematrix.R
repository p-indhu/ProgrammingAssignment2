
## Matrix inversion is a costly computation.  
## The functions 'makeCacheMatrix' and 'cacheSolve' together avoid repeatedly computing the inverse of a matrix.  
## When the inverse of a matrix is computed it is stored(cached) in a special global variable.
## Hence, without any change to the matrix, if the 'cacheSolve' method is called, cached value of inverse is returned.


## Creates a special object, giving access to functions that set and get the matrix value('x') and its inverse('i')
## Super assignment opertator '<<-' makes the scope of the variable 'x' and 'i' global.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## Check if the inverse is already computed. If so, return the inverse('i').  
## If not, inverse if computed using 'solve()', stored in the global variable 'i' using 'setinverse()' function and returned.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached value of inverse matrix")
                return(i)
        }
        
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
        ## Returning a matrix that is the inverse of 'x'
}
