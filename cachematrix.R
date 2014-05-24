## Reads in a matrix into the cache and computes the inverse
## If the inverse is stored in the cache, it is retrieved and returned
## Otherwise, the inverse is computed and returned

## Initializes the cache to NULL and creates a function to
## solve for the inverse of an input matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function (y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function () m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Creates a function to check the cache for an existing inverse matrix
## If the inverse is found in the cache, this is returned
## If the cache is empty, the inverse of the input matrix is calculated

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return (m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
