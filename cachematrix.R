##The two following functions can create a special object, which is matrix, 
##calculate it's inverse matrix and caches the value. It is useful as calculatein 
##inverse matrix is time-consuming procedure, and it is unreasonable to do it 
##multiple times with the same object.


## The first function makes special object, which is actually a list, 
##that contains a functions to set the value of this special object (matrix), 
##get it, set the value of inverse matrix and get it. 

makeCacheMatrix <- function(x = matrix()) {
  matr <- NULL
  set <- function(y) {
    x <<- y
    matr <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) matr <<- solve
  getsolve <- function() matr
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function checks whether the inverse matrix exists in cache and returns it 
## from cache if so, and calculates it with "solve" function if no.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matr <- x$getsolve()
  if(!is.null(matr)) {
    return(matr)
  }
  matr <- x$get()
  output <- solve(matr, ...)
  x$setsolve(output)
  output
}
