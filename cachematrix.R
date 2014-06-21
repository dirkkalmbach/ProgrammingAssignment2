## The Code contains of the two functions 'makeCacheMatrix' and 'cacheSolve', 
## which calculates the inverse of a matrix and stores it result in a variable.
## Whenever this matrix will be inverted, the code returns the stored value
## instead of calculating the inverse again. 

## This function inputs a matrix and outputs a list of functions to set and get
## the value of this matrix as well as the inverse of this matrix.

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


## This function inputs a matrix and checks if this matrix has been already
## inverted. If not it calculates and outputs it's invert, otherwise it outputs
## the stored value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
