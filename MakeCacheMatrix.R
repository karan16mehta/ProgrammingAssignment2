makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y){
    x <<- y
    z <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {z <<- inverse}
  getInverse <- function() {z} 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  
  z <- x$getInverse() ## Return a matrix that is the inverse of 'x'
  if(!is.null(z)){
    message("getting cached data")
    return(z)
  }
  mat <- x$get()
  z <- solve(mat,...)
  x$setInverse(z)
  z
}