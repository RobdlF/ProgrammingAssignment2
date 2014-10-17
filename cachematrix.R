## The function below sets a matrix 'x' and calls it. It also calculates and calls the inverse of 'x'

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {   ## The matrix 'x' is defined as the argument
                x <<- y        ## for the 'set' function in this environment
                s <<- NULL     ## 's' has no defined value at this point
        }
        get <- function() x                         ## Calls the function
        setsolve <- function(solve) s <<- solve     ## Calculates the inverse 's'
        getsolve <- function() s                    ## of the matrix 'x'
        list(set = set, get = get,                  ## Tests the objects and
             setsolve = setsolve,                   ## whether they match
             getsolve = getsolve)
}


## The next function looks for the inverse of 'x' in the cache. If it does not exist yet, the function solves the
## matrix 'x' and returns its inverse:

cacheSolve <- function(x, ...) {
        s <- x$getsolve()                         ## Asks for the inverse matrix of 'x'
        if(!is.null(s)) {                         ## If the result of the previous operation is not the inverse of 'x',
                message("getting cached data")    ## then R 'looks in the cache'
                return(s)                         ## and returns the inverse of 'x'
        }
        data <- x$get()                           ## Retrieves the given matrix 'x' from the environment
        s <- solve(data, ...)                     ## Defines the inverse 's' of 'x'
        x$setsolve(s)                             ## Calls the inverse of 'x'
        s                                         ## Returns the matrix that is the inverse of 'x'
}
