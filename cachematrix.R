# R-PROGRAMMING ASSIGNMENT 2

# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# first of the following two functions creates a special "matrix" object that 
# can cache its inverse. The second function then computes the inverse of the 
# special "matrix" returned by makeCacheMatrix. If the inverse has already been 
# calculated (and the matrix has not changed), then the cachesolve should 
# retrieve the inverse from the cache.

# An example of the usage of these two functions is as follows:
# These may be entered into the prompt console in RStudio
# > x <- matrix(rnorm(4), nrow = 2)           # Create a 2x2 square matrix named x
# > smx <- makeCacheMatrix(x)                 # Create the special matrix
# > smx$get()                                 # Return the original square matrix
# > cacheSolve(smx)                           # Return the inverse of the matrix
# > cacheSolve(smx)                           # It now returns the cached inverse

#############################################################################
# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                                   # inv will store the cached 
                                                # inverse 
  
  set <- function(y) {                          # Set for the matrix
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x                            # Get for the matrix
  
  
  setinv <- function(inverse) inv <<- inverse    # Set for the inverse
  
  getinv <- function() inv                       # Get for the inverse
  
  # Return the list with our newly defined functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

#############################################################################
# cacheSolve: if the furnction, which compute the inverse of the matrix. 
# If the inverse is already calculated before, it returns the cached inverse.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  # If the inverse has already been calculated, it will return it
  if (!is.null(inv)) {                            
    message("getting cached data")
    return(inv)
  }
  
  # If the inverse has not yet been calculated, then we shall calculate it
  data <- x$get()
  inv <- solve(data, ...)
  
  # The calculated inverse is now Cached
  x$setinv(inv)
  
  # Return the inverse
  inv
}