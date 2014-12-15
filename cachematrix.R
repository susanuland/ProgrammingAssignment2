## Matrix inversion is usually a costly computation. These functions will
## allow us to cache a matrix inverse rather than computing it repeatedly
## Note: we assume that the matrix supplied is always invertible

## makeCacheMatrix function creates a special matrix object that 
## can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
     p <- NULL
     set <- function(z) {
       x <<- z
       p <<- NULL
     }
     get <- function() x
     setinv <- function(inv) p <<- inv
     getinv <- function() p
     list( set = set, get = get, setinv =setinv, getinv=getinv)
}


## cacheSolve function computes the inverse of the special matrix 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated ( and the matrix has not changed )
## then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
       
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get( )
  m <-solve(data, ...)
  x$setinv(m)
  m
}
