## Overall Description ##

# The following functions below take a stored, squared matrix (assumed to be
# invertible), calculates the inverse of the matrix, and caches it for later
# use.


## makeCacheMatrix ##

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set,
         get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve ##

# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then cacheSolve should retrieve the inverse from 
# the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("We have the data!  Here ya go:")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}


## Testing ##

matr <- matrix(1:4, nrow = 2, ncol = 2)
solve(matr)

myMatrix <- makeCacheMatrix(matr)
myMatrix

cacheSolve(myMatrix)    # It works!  At least, for a 2 x 2 matrix...