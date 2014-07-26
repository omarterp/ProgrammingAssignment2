## Maintains a special object which allows the assignee the ability to create an invertible matrix and cache it

## Creates a reference to the object for the given matrix and creates helper functions to set, cache and get its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  invMatrix <- NULL
  
  set <- function(y) {
    
    x <<- NULL
    invMatrix <<- NULL
  }
  
  get <- function() x
  setInverted <- function(invertedMatrix) invMatrix <<- invertedMatrix
  getInverted <- function() invMatrix
  
  list(get = get, set = set, getInverted = getInverted, setInverted = setInverted)
}

## Returns the given matrix inverted. An equality check is performed on a cached global variable.

## If the special matrix already has its inverted value cached, then it is returned from the global
## variable - invertedMatrix.

## I chose to check for the null value and change flow to proceed to the last line of the function, 
## instead of using a return before the function block ends. The function flows better this way, in my opinion.
cacheSolve <- function(x, ...) {
  
  invMatrix <- x$getInverted()
  
  if(is.null(invMatrix)) {
    
    print("Setting inverted Value.")
    
    invertedMatrix <- solve(myMatrix, ...)
    invMatrix <- invertedMatrix
    x$setInverted(invMatrix)
    
  } else {
    print("Getting inverted matrix from cache.")
    
    invMatrix <- x$getInverted()
  }
  
  invMatrix
}