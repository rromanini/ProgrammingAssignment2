## Create a matrix, calculate the inverse and put in the cache
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function search the cache for the inverse of matrix
## if not found, use the solve function to calculate inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## solve(A) Inverse of A where A is a square matrix.  
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
