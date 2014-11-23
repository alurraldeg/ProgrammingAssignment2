## The functions are very similar to the ones of the examples

## makeCacheMatrix returns an object with the following functions:
## set: sets the object's matrix and resets the cache
## get: returns the actual matrix stored in the object's attribute
## seti: stores the matrix's inverse
## geti: returns the cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  seti <- function(inv) m <<- inv
  geti <- function() m
  list(set = set, get = get,
       seti = seti,
       geti = geti)
}


## ask for the inverse stored in the object. If there is no cache then
## ask for the actual matrix and calculates the inverse. After that,
## sets the cache

cacheSolve <- function(x, ...) {
    m <- x$geti()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$seti(m)
    m
}
