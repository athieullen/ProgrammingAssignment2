## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  In <- NULL
  set <- function(y) {
    x <<- y
    In <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) In <<- inverse
  getinverse <- function() In
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of a special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  In <- x$getinverse()
  if(!is.null(In)) {
    message("getting cached data")
    return(In)
  }
  data <- x$get()
  In <- solve(data)
  x$setinverse(In)
  In
}
