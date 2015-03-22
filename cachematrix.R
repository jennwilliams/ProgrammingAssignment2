# The following two functions cache the inverse of a matrix. This is done because matrix 
# inversion is usually a costly computation. Therefore, there may be some benefit to 
# caching the inverse of the matrix, instead of repeatedly computing it. 


##The first function, `makeCacheMatrix` creates a special "matrix" object, which is
#really a list containing a function to
#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse of the matrix
#4.  get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse_x <- NULL
  set <- function(y) {
    x <<- y
    inverse_x <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse_x <<- inverse
  getinverse <- function() inverse_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function calculates the inverse of the special matrix created in makeCacheMatrix. 
# But, first it checks if the inverse matrix has previously been computed. If it has, it skips 
# the computation and simply retireves it from the cache and returns it. If it has not, it 
# computes the inverse of the matrix, caches it, and returns it.  

cacheSolve <- function(x, ...) {
  x_inverse <- x$getinverse()
  if(!is.null(x_inverse)) {
    message("getting cached inverse matrix")
    return(x_inverse)
  }
  data <- x$get()
  x_inverse <- solve(data, ...)
  x$setinverse(x_inverse)
  x_inverse
}
