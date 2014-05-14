
## General description: Returns the Inverse of a matrix, either by calculation, 
## or if already calculated, from the cache

## Creates a list with 4 elements called: $set  $get  $setmean  $getmean
## To be used by cacheSolve to decide to solve or return cached value


makeCacheMatrix <- function(x = matrix()) {

  ##Ensure m is empty to start
  
  m <- NULL
  
  ## defines the set element of the list, which is a function, which sets x as y (matrix passed to function), 
  ## and sets m as NULL and both are available in the parent environment
    
  set <- function(y) {
    x <<- y 
    m <<- NULL
  }
  
  get <- function() x   ## the get function in the list returns the value of x (the matrix)
  setinverse <- function(solve) m <<- solve   ## setinverse sets m to be the inverse, and makes available in parent
  getinverse <- function() m   ## the getinverse returns the value of m, the inverse or NULL
  
  ## Finally creates the list to be returned
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns the Inverse of the matrix, either from the cache, or via solving it.

cacheSolve <- function(x, ...) {
  
  ## Purpose: Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()  ## Assign m to be the value returned from the getinverse function, defined in the makeCAcheMAtrix function

  ## Checks if variable m is empty. If m is not empty, tells user that using cached data
  ## and returns cached data (from m)
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## If inverse not in cache, need to solve
  
  data <- x$get()   ## Puts value of matrix into data
  m <- solve(data, ...)   ## puts inverse of the matrix into m
  x$setinverse(m)     ## stores the value of the inverse in the list in makeCacheMatrix
  m     ##returns m
        
}
