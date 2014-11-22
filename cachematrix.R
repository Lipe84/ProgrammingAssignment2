# This is the first function: makeCacheMatrix
# It will create a special object, a list of 4 elements (set, get, setsolve, getsolve)

makeCacheMatrix <- function(x = matrix()) {         # Variable "X" will be a invertible matrix
    inverse <- NULL                                   # Variable "inverse" will be our our inverted-matrix:
    # it is set to "NULL" every time that makeCacheMatrix is called
    
    set <- function(y) { 
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() { x }                           # this function returns the value of the original matrix "x"
    
    
    setsolve <- function(solve) {                     # this is called by cacheSolve() function during the first cacheSolve()
        inverse <<- solve                               # access and it will store the value using superassignment <<-
    }
    
    getsolve <- function() {                          # this will return the cached value to cacheSolve() on subsequent accesses (it doesn't have input argument)
        inverse
    }
    
    list(set = set, get = get,                        # this is the list of the internal function of the function "makeCacheMatrix"
         setsolve = setsolve,
         getsolve = getsolve)
}

###################################################
# This is the second function: cacheSolve
# It calculates the inverse of a given matrix and returning the screen message "Inverted matrix already stored in cache" if the computation was already performed

cacheSolve <- function(x, ...) {                    # the input "x" is an object created by makeCacheMatrix (a 4-list)
    inverse <- x$getsolve()                           # assign to "inverse" variable the item "getsolve" of the list "x": it is the function SOLVE
    
    if(!is.null(inverse)) {                                 # "if" condition checks if there is already a value stored in the variable "inverse":
        message("Inverted matrix already stored in cache")    # - if positive, it returns a screen message with "Inverted matrix already stored in cache" and finish the job
        return(inverse)                                       # - if negative, continue...
    }
    
    data <- x$get()                                   # if "inverse" is null, then I pass to "data" the value "get" of the list "x" (that was the Matrix inserted from outside)
    inverse <- solve(data, ...)                       # finally, compute the inverse of the matrix by "solve"
    x$setsolve(inverse)                               # store the calculated inverse matrix in "x" using the item of the list "x", called setsolve: in this way, I saved the result of
    inverse                                           # of matrix inversion and returning it
}