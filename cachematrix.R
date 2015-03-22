## The goal of these functions is to be able to cache the inverse of a matrix
## by creating a "special matrix" that is actually a list of functions to access
## the matrix and its inverse, and also to get and set the corresponding values

## makeCacheMatrix creates a list that contains a matrix, its inverse
## and two functions to get and set the inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## The first step is to set the value of the inverse to NULL
  inv <- NULL
  
  ## By calling set() function you change the original matrix and set 
  ## the cached inverse to null. This step is necessary to force the 
  ## recalculation of the inverse after changing the matrix
  ## No actual calculation is done here
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## get() returns the value of the matrix
  get <- function() x
  
  ## setinverse() assigns a new value to the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
  ## getinverse() returns the inverse of the matrix
  getinverse <- function() inv
  
  ## finally, a list is created which contains the four previously
  ## declared functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve calculates the inverse of a matrix using the list returned by makeCacheMatrix
## If the inverse has already been calculated, the function uses that value
## instead of calculating it again

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## First there's a call to getinverse() to get the stored value of the inverse
  ## of the matrix
  i <- x$getinverse()
  
  ## if the inverse is not null, i.e., it is already in cache, it is
  ## immediately returned and the function execution stops here
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## Otherwise, if the inverse was null, i.e., it wasn't previously calculated
  ## get() function is used to retrieve the value of the matrix and
  ## solve() function is used to calculate its inverse and store the value in cache
  ## by using setinverse() function of the special matrix
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  
  ## And finally the inverse of the matrix is returned
  i
}
