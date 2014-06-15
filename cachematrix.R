## Put comments here that give an overall description of what your
## functions do

## the makeCacheMatrix function creates 4 more functions "set" , "get", 
## "setinv" and "getinv" that are then 
## used to create a matrix and compute the inverse.

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function checks if there is a cached result for the required matrix, if one
## is found then it returns that value, if not it calculates the inverse again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
