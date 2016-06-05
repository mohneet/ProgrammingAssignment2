## Writing functions that can store the inverse of a matrix in 
## cache, so it can be quickly recalled, if needed.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}

## This function computes the inverse of the matrix returned by the function above. ## If the inverse is already in cache, then it's retrieved, else computed fresh.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  message("calculating the inverse, no cached value available")
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
