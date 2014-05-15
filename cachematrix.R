
## General description: Returns the Inverse of a matrix, either by calculation, 
## or if already calculated, from the cache

## First Function: Creates a list with 4 elements called: $set  $get  $setmean  $getmean
## To be used by cacheSolve to decide to solve or return cached value

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL                   ## Ensure m is NULL to start
    
  set <- function(y) {        ## Defines the variable "set" as a function, which resets the function 
    x <<- y                   ## which sets x as y (matrix passed to function),
    m <<- NULL                ## and sets m as NULL and both are available in the parent environment
  }                           ## Can be called to reset the function, to use a new matrix (remove old inverse)
  
  get <- function() x                         ## sets the "get" variable to be the value x.
                                              ## it uses an annonymous function, that just returns x (the matrix)
  setinverse <- function(solve) m <<- solve   ## sets the "setinverse" variable to be the inverse, and makes available in parent
                                              ## if m is NULL, so is setinverse
  getinverse <- function() m                  ## sets the "getinverse" value in the list as value of m (the inverse or NULL)
                                              ## it uses an annonymous function, that just returns m 
  
  ## Finally creates the list to be returned (as last value it is returned)
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Second Function. Input is the list that is returned from teh makeCacheMatrix Function.
## Returns the Inverse of the matrix entered into makeCacheMatrix, either from the cache, or via solving it.

cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()   ## Assign m locally to be the value held in the list x, position "getinverse"
                        ## defined in the makeCAcheMAtrix function - either the inverse or NULL

  ## Checks if variable m is NULL If m is not NULL, tells user that using cached data
  ## and returns cached data (from m)
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## If inverse not in cache, need to solve
  
  data <- x$get()         ## Puts value of matrix into data, from the list x (passed to cacheSolve function), 
                          ## position "get", defined in the makeCacheMAtrix function
  m <- solve(data, ...)   ## calculates inverse via solve function and then puts results into m
  x$setinverse(m)         ## stores the value of the inverse in the list created by makeCacheMatrix
                          ## in the "setinverse" position. This also sets m globally to be the inverse.
                          ## which means when run again, m is not NULL
  m                       ## returns m
        
}
