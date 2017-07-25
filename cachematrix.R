## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## creates a special "matrix" object that is able to cache it's inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInv <- function(inv) i <<- inv
  getInv <- function() i
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function
## computes the inverse of of the special matrix retuned by makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInv(i)
  i
}
