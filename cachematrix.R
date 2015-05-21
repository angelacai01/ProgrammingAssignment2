# Matrix inversion is usually a costly computation and there may be some benefit
# to cache the inverse of a matrix rather than compute it repeatedly. 

# The makeCacheMatrix function creates a list containing a function to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  #set function: assign value of y to x which exists globally via << operator
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  #get function: get the value of x
  get <- function() x
  #setsolve function: assign value of inverse matrix to s that exists globally
  setsolve <- function(solve) s <<- solve
  #getsolve function: get the value of s
  getsolve <- function() s
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
# It assumes that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
  #get the inverse of the matrix value from cache
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  #if nothing is returned from cache, calculate a new inverse of matrix value and store into cache
  message("calculate inverse matrix and store into cache")
  data <- x$get()
  s <- solve(data)
  #store the caculated data into caches
  x$setsolve(s)
  s
}
