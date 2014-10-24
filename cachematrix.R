
## This function caches a matrix inverse. It contains four functions to 
## set and get a matrix, and to set and get its inverse.
## After running this function, its value can be used as
## argument to cacheSolve function to initiate caching of
## the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
m<- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function takes the value of makeCacheMatrix 
## as argument, and its value is the inverse of the 
## matrix given as argument to makeCacheMatrix function.
## First time this function is called it will calculate
## the matrix inverse. On subsequent calls, it will get 
## the inverse from cache, and output the message 
## 'getting cache data'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
