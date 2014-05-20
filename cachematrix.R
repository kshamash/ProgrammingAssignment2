##R Programming: Assignment 2


## Caches the inverse of a matrix rather than computing it many times

## makeCacheMatrix creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL ##cached inverse
  
  ##NESTED FUNCTIONS 
  
  #sets the value of x to a new matrix and resets the inverse
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  
  #returns the value of x
  get <- function() x
  
  #sets the cached inverse of x
  setinverse <- function(inverse) i<<-inverse
  
  #returns the cached inverse
  getinverse <- function() i
  
  
  ##RETURN VALUE
  #returns a list of all the functions of makeCacheMatrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}


## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
  
  #gets the cached inverse of x
  i <- x$getinverse()
  
  #if i has already been set, no need to calculate it again!
  #just return the value of i and exit the function
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  
  #otherwise calculate the inverse and assign it to i
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  
  #return i
  i
}





