# Pair of functions, the first producing a matrix that can cache its inverse
# the second computes the inverse, or if it has already been done and is the same
# returns the value from the cache

# Produce a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, 
       get=get, 
       setinv=setinv, 
       getinv=getinv)
}


# calculate and return the inverse of the matrix created with makeCacheMatrix. Input is the 
# object where you saved the makeCacheMatrix function

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  inv = x$getinv()
  #Check to see if it's already been calculated
  if (!is.null(inv)){
  #Message to let you know it has and that it's using cached data
    message("using pre-cached data")
    return(inv)
  }
  #Oh, it hasn't? better do it now!
  matrixdata = x$get()
  inv = solve(matrixdata, ...)
  x$setinv(inv)
  return(inv)
}
