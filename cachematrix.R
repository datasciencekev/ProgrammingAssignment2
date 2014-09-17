## These two functions work to build a matrix object 
## and to calculate and cache/recover the inverse



#This is the function to make the matrix to get cached
#It's based on the sample code in the assignment description.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


#This is the function that calls the matrix function - it either 
#calculates the inverse of the matrix, or pulls it from the cache if the
#cache appears to contain a valid solution.
#


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}