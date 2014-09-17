## cachematrix.R:
##   This file contains the functions makeCacheMatrix and cacheSolve,
##   which take advantage of the <<- operator to cache the value
##   of the inverse of a matrix (calculated using the solve function) 
##   so that it only has to be computed once. These functions follow the 
##   example of "makeVector" and "cachemean", as given in 
##   https://github.com/rdpeng/ProgrammingAssignment2/blob/master/README.md

##  `makeCacheMatrix`: 
##     This function creates a special "matrix" object
##     that can cache its inverse. 
makeCacheMatrix <- function(x = matrix()) {
     # There are two variables in this object:
     # x (the matrix), and inv (its inverse). 
     # x is an argument to the function, and 
     # initially we want inv to be NULL.
     inv <- NULL
     
     # "set" sets the value of x and resets inv to NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     
     # "get" returns the value of x
     get <- function() x
     
     # "setinv" sets the value of inv
     setinv <- function(inverse) inv <<- inverse
     
     # "getinv" returns the value of inv
     getinv <- function() inv
     
     # the return value for MakeCacheMatrix is a list of 
     # the four functions set, get, setinv, and getinv
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


## `cacheSolve`: 
##   This function computes the inverse of the special
##   "matrix" returned by `makeCacheMatrix` above. If the inverse has
##   already been calculated (and the matrix has not changed), then
##   `cacheSolve` retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
     # try to get the inverse matrix
     # if it is not null, then the inverse was already 
     # calculated and cached, and we want to return it
     m <- x$getinv()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     
     # if we get here, the cached inverse was NULL, so 
     # we need to retrieve the original matrix, 
     # calculate its inverse using solve,
     # cache the inverse, 
     # and then return it
     data <- x$get()
     m <- solve(data, ...)
     x$setinv(m)
     m
}
