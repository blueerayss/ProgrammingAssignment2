## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}

makeCacheMatrix <- function(x = matrix()) {
  # Initialize inverse to NULL
  inv <- NULL
  
  # Custom object to hold matrix and inverse
  matrix <- list(
    # Set the matrix value
    set = function(y) {
      x <<- y
      inv <<- NULL  # Reset inverse when matrix changes
    },
    # Get the matrix value
    get = function() x,
    # Set the inverse value (for internal use)
    setinverse = function(inverse) inv <<- inverse,
    # Get the cached inverse
    getinverse = function() {
      if (is.null(inv)) {
        # Calculate inverse only if not already cached
        inv <- solve(x)
      }
      return(inv)
    }
  )
  
  # Return the custom object
  return(matrix)
}

cacheSolve <- function(matrix) {
  # Get the inverse from the object's getinverse method
  return(matrix$getinverse())
}

