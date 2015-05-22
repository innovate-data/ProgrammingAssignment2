## The functions makeCacheMatrix and cacheSolve togather implement a caching functionality for the inverse of a square matrix.

## makeCacheMatrix function creates a list of function associated with the matrix.
## The functions are get, set , getinverse and setinverse.

makeCacheMatrix <- function(x = matrix()) 
{
  ## Initialize the matrix inverse to NULL while creating the list
  inv <- NULL
  
  ## set method sets the value of the matrix
  set <- function(y) 
  {
    # set the matrix value to the value provided to set function call
    x <<- y
    
    # Reset the matrix inverse to NULL since the matrix got a new value and the inverse needs to be computed again
    inv <<- NULL
  }
  
  ## get method returns the matrix value
  get <- function() x
  
  ## setinverse method sets the value of the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
  ## getinverse method gets the value of the inverse of the matrix
  getinverse <- function() inv
  
  ## return a list of the methods defined above
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function returns a matrix that is the inverse of 'x' 
## It computes the value of inverse of the matrix supplied as argument.
## Once the inverse is computed, this function will cache the matrix inverse value,
## so that next time the inverse is required, it will be retrived from the cache instead of computing it all over again

cacheSolve <- function(x, ...) 
{
  ## get the matrix inverse using the list method defined in above function 
  inv <- x$getinverse()
  
  ## check if matrix inverse is in cache
  if(!is.null(inv)) 
  {
    message("getting cached data")
    ## if matrix inverse is in cache, return the cached value
    return(inv)
  }
  
  ## if matrix inverse is not in cache, compute the matrix inverse and cache it.
      ## 1.get the matrix
      data <- x$get()
      ## 2.compute the inverse of the matrix
      inv <- solve(data)
      ## 3.cache the inverse of matrix
      x$setinverse(inv)
  
  ## return a matrix that is the inverse of 'x'
  inv  
}
