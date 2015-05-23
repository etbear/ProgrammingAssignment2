## the two functions allow us to solve a matrix and store the solution for future use
## when called the first time, cacheSolve will solve the matrix
## calling cacheSolve later will retrieve previously stored solution, eliminating the need to re-compute

## makeCacheMatrix returns a list of 4 objects, set, get, setinv and getinv
## set is used to store matrix value; get returns the matrix
## setinv is used to store inverse matrix; getinv returns the inverse matrix (or null)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve takes a list as argument
## it checks if inverse of a matrix has been previously computed
## if so, the function retrieves the stored value without further computation
## otherwise, the function solves the matrix and set the value at the same time

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
