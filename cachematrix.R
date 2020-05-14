##This functions will first create a list for storing a matrix and its inverse in cache 
##to then calculate ist inverse only if it has not been calculated before


## Creates a list that stores the matrix given
## and will store its inverse (when calculated using cacheSolve)

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
    }
  get <- function() x
  setinv<- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## will calculate the iverse of a matrix stored by makeCacheMatrix
## only if it has not been calculated before

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(i)
  i
}
