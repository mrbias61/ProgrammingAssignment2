## Put comments here that give an overall description of what your
## functions do

## Setting up a function to cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inv<-NULL
  set<-function(y) {
    x<<-y
    inv<<-NULL
  }
  get<- function() x
  setInverse<-function(inverse) inv<<-inverse
  getInverse<-function() inv
  list(set=set,
       get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}


## Checking to make sure that the function works by solving the cached function and returning the inverse matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  inverse_matrix<-x$get()
  inv<-solve(inverse_matrix, ...)
  x$setInverse(inv)
  inv
  
}
