## makeCacheMatrix creates a "special" matrix which will enable cache storage
## cacheSolve will check in cache first for the inverse function, if not
## then it will compute

## To create a special list for the cache storage of the matrix and the 

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) 
  {
    x <<- y
    m <<- NULL
  }
  
  get <- function() 
    x
  
  setsolv <- function(solv) 
    m <<- solv
  
  getsolv <- function() 
    m
  
  list(set = set, get = get, setsolv = setsolv, getsolv = getsolv)

}


## Write a function to return the inverse of matrix

cacheSolve <- function(x, ...) {
  
  m <- x$getsolv()
  
  if(!is.null(m)) 
  {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data)
  x$setsolv(m)
## Return a matrix that is the inverse of 'x'
  m
}
