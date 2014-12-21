## This pair of functions is used to cache the inverse of a matrix 
## rather than compute it repeatedly.
## If the inverse has already been calculated and the matrix has not 
## changed since then, the inverse is retrieved from the cache.

## This function creates a special "matrix" object that can cache
## its inverse, taking advantage of the scoping rules of R language.
## Literally, this object is a list containing a function for each of 
## the following operations:
## 1. Setting the value of the matrix - set(y = matrix())
## 2. Getting the value of the matrix - get()
## 3. Setting the value of the inverse - setInverse(m = matrix())
## 4. Getting the value of the inverse - getInverse()

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
		## If the matrix is changed, the cached inverse is cleared
        inverse <<- NULL
    }
    get <- function() {
        x
    }
    setInverse <- function(m) {
        inverse <<- m
    }
    getInverse <- function() {
        inverse
    }
    list(set = set, 
         get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


## This function calculates the inverse of the special "matrix" 
## created with the makeCacheMatrix() function.
## It first checks if the inverse has already been calculated, in which
## case it gets the inverse from the cache and skips the computation.

cacheSolve <- function(x, ...) {
    inverseMatrix <- x$getInverse()
    if(!is.null(inverseMatrix)) {
        ## Getting cached inverse matrix
        return(inverseMatrix)
    }
    mat <- x$get()
    ## Calculating inverse matrix
    inverseMatrix <- solve(mat, ...)
    x$setInverse(inverseMatrix)
    ## Return a matrix that is the inverse of 'x'
    inverseMatrix
}
