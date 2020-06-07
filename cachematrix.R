## makeCacheMatrix is a function meant to
## Set the values of a Matrix, Get the values of a matrix, 
##Set the inverse values of a matrix, and get the inverse values of a matrix
makeCacheMatrix <- function(x = matrix()) {
  
  M <- NULL
  
  set <- function(y) {
    x <<- y
    M <<- NULL
  }
  get <- function() {x}
  
  setINV <- function(inverse) {M <<- inverse}
  
  getINV <- function() {M}
  
  list(set = set, get = get,
       setINV = setINV,
       getINV = getINV)
}

## CacheSolve will use the solve function to generate the inverse of the previously generated matrix if not already cached;
##if already cached, retrieve from cache.

cacheSolve <- function(x, ...) {
        
  M <- x$getINV()
  if(!is.null(M)) {
    
    message("Procuring cached data")
    
    return(M)
  }
  
  mtrix <- x$get()
  
  M <- solve(mtrix, ...)
  
  x$setINV(M)
  
  M
}
