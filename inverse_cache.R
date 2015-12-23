# makeCacheMatrix creates a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# The cacheSolve calculates the inverse of the matrix created with the above function.
# However, it first checks to see if the inverse has already been calculated.
# If so,it gets the inverse from the cache and skips the computation else it will calculate the inverse.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse metrics")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## sample run -1 

# t <- s<-makeinverse(matrix(rnorm(16),4,4))
# t$get()                   # showing created matrix t (4X4)
# cacheinverse(t)           # calculate inverse matrix of t   
# cacheinverse(t)           # Return inverse matrix from cache with message "getting cached inverse metrics
# t$get()                   # showing created matrix 

## sample run -2

# y<-makeinverse(matrix(1:4,2,2))
# y$get()                   # showing created matrix y (2X2)
# cacheinverse(y)           # calculate inverse matrix of y   
# cacheinverse(y)           # Return inverse matrix from cache with message "getting cached inverse metrics
# y$get()                   # showing created matrix 
