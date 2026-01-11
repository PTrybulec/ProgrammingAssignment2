## These functions create a special matrix object that can cache
## its inverse. Caching the inverse avoids recomputing it when the
## matrix has not changed.

## makeCacheMatrix creates a special matrix object with methods to
## set and get the matrix, as well as to set and get its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  
  getinverse <- function() inv
  
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

## cacheSolve computes the inverse of the matrix returned by
## makeCacheMatrix. If the inverse has already been computed,
## it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  if (!is.null(inv)) {
    return(inv)
  }
  
  specialMatrix <- x$get()
  inv <- solve(specialMatrix, ...)
  x$setinverse(inv)
  
  inv
}
