## This function is to compute the inverse of a matrix, and stores the result in a cache
##Usage: hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
# h8 <- hilbert(8); h8 #generate a 8*8 matrix
# b <- makeCacheMatrix(h8) #define b as makeCacheMatrix
# b$get()
# cacheSolve(b)

## Generate a list contain set, get, setinverse and getinverse

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## solve the inverse of a matrix, and store the result in the cache. If the result is already in the cache, get it
## directly

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse();
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
  
}
