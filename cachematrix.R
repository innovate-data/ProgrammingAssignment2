## The functions togather implement a caching functionality for the inverse of a matrix.

## makeCacheMatrix function creates a list of function associated with the matrix. The functions are get, set , getinverse and setinverse.
## get and set function get and set the value of the matrix respectively
## setinverse method sets the value of the inverse of the matrix
## getinverse method gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
  # Initialize the matrix inverse to NULL while creating the list
  inv <- NULL
  
  # set function
  set <- function(y) 
  {
    # set the matrix value to the value provided to set function call
    x <<- y
    
    # Reset the matrix inverse to NULL since the matrix got a new value and the inverse needs to be computed fresh
    inv <<- NULL
  }
  
  # get function : return the matrix value
  get <- function() x
  
  # setinverse function 
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  
  if(!is.null(inv)) 
  {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  
  inv <- solve(data)
  
  x$setinverse(inv)
  
  inv  
}
