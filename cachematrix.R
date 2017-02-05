## The functions below allow you to set a matrix and solve for the inverse of the matrix. It appears that we are setting 
## X outside of the enclosing environment? Which I believe allows cacheSolve to call it?
## 

## makeCacheMatrix allows you to set x to a matrix and stores the matrix outside of teh enclosing environment

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  seti <- function(inverse) i <<- inverse
  geti <- function() i
  list(set = set, get = get,
       seti = seti,
       geti = geti)
}


## Solves for makeCacheMatrix returning the inverse of the matrix. If the matrix is not changed it should retrive it
## from cache

cacheSolve <- function(x, ...) {
 i <- x$geti()
  
  if(!is.null(i)) {
    
    message("getting cached data")
    return(i)
  
  }
  data <- x$get()
  i <- solve(data, ...)
  x$seti(i)
  i
}

