## These functions allow one to cache a matrix and it's inverse.
## If the inverse has not already been calculated, it will calculate
## the inverse and cache for later use.

## This function takes a matrix and returns a list of four functions
## These functions are used to set the cached matrix, return the cached
## matrix, set the inverse of the matrix and get the cached inverse
## of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes the list of functions created by makeCacheMatrix.
## If the inverse has been cached, it returns the inverse
## If the inverse has not been cached, it calulates the inverse using
## the solve() function, stores the calculated inverse in the cache
## and returns the inverse of the matrix.

cacheSolve <- function(x) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i<-solve(data)
  x$setinverse(i)
  i
}
