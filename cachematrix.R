## Put comments here that give an overall description of what your
## functions do

## Create a Matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ## Reset the matrix x and m
  set <- function(y){
    x <<- y
    m <<- null
  }
  
  ## return the matrix x
  get <- function() x
  
  ## set the inverse of matrix x
  setinverse <- function(inverse) m <<- inverse
  
  ## get the inverse of the matrix x
  getinverse <- function() m
  
  ## return list of operations/functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {

  ## retrieve the cached matrix inverse
  m <- x$getinverse()
  
  ## check if the matrix inverse is cached
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## retrieve the matrix
  data <- x$get()
  
  ## inverse the matrix
  m <- solve(data, ...)
  
  ## cache the inverse of the matrix
  x$setinverse(m)
  
  ## return the inverse of the matrix
  m
}
