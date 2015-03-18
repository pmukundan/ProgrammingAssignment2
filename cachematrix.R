## makeCacheMatrix returns a list that contains 4 functions
## set: sets the matrix
## get: returns the matrix
## setinverse: sets the value of the variable i to the value passed to it
## getinverse: returns the value of the variable i. 



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


## cacheSolve expects a list created by makeCacheMatrix as the argument
## The first time the function is called for a particular variable, x$getinverse will return null
## the function will then use solve to calculate the inverse and then call x$setinverse to store it in cache
## the next time it is called with the same variable, it will return the value and won't need to recalculate

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("Here I am - getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
