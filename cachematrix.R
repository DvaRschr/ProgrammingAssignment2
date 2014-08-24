## Programming Assignment No. 2 is going to involve 
## the use of the "solve" funtion.

## Write a short comment describing this function

## Our class is using the "cache" function to 
## make a special matrix object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  
  ## The makeCacheMatrix function creates a 
  ## special matrix, which is really a list
  ## containing a function to set the value of
  ## the matrix; to get the value of the matrix;
  ## to set the value of the matrix's inverse;
  ## and to get the value of the matrix's inverse.
  
  set <- function(y){
    x <<- y
    s <<- NULL
  } #set
  
  get <- function() {x}
  setInverse <- function(inverse) {s <<- inverse}
  getInverse <- function() {s}  
  
  
  list(set = set, get = get, setInverse = setInverse,
       getInverse = getInverse)
  
  ## We will always assume in this assignment that the
  ## matrix supplied is always invertible.
  
} ## makeCacheMatrix


## Write a short comment describing this function

## Our class is using the "solve" function to 
## calculate the inverse of a square matrix value 
## that is returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## Note that if the inverse has already been 
  ## calculated (and the matrix is unchanged)
  ## then the cacheSolve function should
  ## retrieve the inverse from the cache.
    
  s <- x$getInverse()
  if(!is.null(s)){
    message("getting cached data")
    return(s)
  } #getInverse
  
  data <- x$get()
  s <- solve(data, ...)
  x$setInverse(s)
  s 
} # cacheSolve
