## These two functions allow the user to solve and cache 
## the inverse of a matrix so that it can be recalled later
## without the need to recalculate it.

## this function enables you to set and get the matrix and to 
## set and get the matrix's inverse.

makeCachematrix<-function(x=matrix()){
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## this function checks to see if the matrix's inverse has already
## been calculated.  If it hasn't, then it calculates and stores
## the inverse.  If it has, then it prints a message and returns the
## stored inverse.  Ta-da!

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
