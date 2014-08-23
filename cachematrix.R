
## makeCacheMatrix reads the argument and store into 
## c_matrix variable the square, non-singular matrix

makeCacheMatrix <- function(x = matrix()) {
  c_matrix <- NULL

  ## set method simply gets the argument y, which  has been 
  ## has been passed to makeCacheMatrix as argument x
  ## and store it into a temp var also called x
  set <- function(y){
    x <<- y
    c_matrix <<- NULL
  }
  
  ## get method simply returns the value of x, assigned on set function
  ## this will always return the original matrix (not inversed)
  get <- function() x
  
  ## setmatrix method pass the function solve (matrix inversion) as argument
  ## and attributes to c_matrix variable initialized above
  ## in this context the c_matrix is now a function
  setmatrix <- function(solve) c_matrix <<- solve
  
  ## getmatrix method returns the inverse matrix
  getmatrix <- function() c_matrix
  
  ## this defines a list of methods
  ## my_obj$get, e.g., wil return the original matrix
  list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## in this line c_matrix is assigned with, in fact, the matrix passed 
  ## as argument for makeCacheMatrix
  ## this wil return an object type matrix
  c_matrix <- x$getmatrix()
  
  ## here we check if the c_matrix has been previously stored or not
  ## if the c_matrix is not NULL means the variable already contains
  ## the cached results
  
  if (!is.null(c_matrix)) {
    message("cached data")
    return(c_matrix)
  }
  
  ## stores in cm variable the initial matrix
  cm <- x$get()
  
  ## calculates the inverse using cm temp var as argument
  c_matrix <- solve(cm, ...)
  
  ## this method stores the inverse matrix
  x$setmatrix(c_matrix)
  
  ## this line just print out the content of assigned matrix
  ## cached or not
  c_matrix
}
