## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  c_matrix <- NULL
  set <- function(y){
    x <<- y
    c_matrix <<- NULL
  }
  
  get <- function() x
  setmatrix <- function(inv_m) c_matrix <<- inv_m
  getmatrix <- function() c_matrix
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
