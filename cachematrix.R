## Function to cache the inverse of a matrix in order to decrease computational demands


## This first function creates a special "matrix" object that can chache its invserve for later use.
## It sets the value of the matrix, gets the value of the matrix, sets the value of the inverse and then gets the value of that inverse. 
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This second function computes the inverse of the special "matrix" returned by the function above.
## If the inverse has already been calculated (and the matrix is unchanged), then this function retrives the cached inverse.
cacheSolve <- function(x, ...){
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  } 
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
