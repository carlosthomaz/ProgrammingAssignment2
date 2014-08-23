## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  c_matrix <- NULL
  set < function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setmatrix <- function(solve) c_matrix <<- solve
  getmatrix <- function() m
  list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  c_matrix <- x$getmatrix()
  if (!is.null(c_matrix)) {
    message("cached data")
    return(c_matrix)
  }
  cm <- x$get()
  c_matrix <- solve(cm, ...)
  x$setmatrix(c_matrix)
  c_matrix
}
