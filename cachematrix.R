## These functions are used to cache the inverse of a matrix. The first function is used to generate a matrix
## that can cache its inverse. The second function is used to compute the inverse of the special "matrix".

## Create a special "matrix" vector based on the input matrix, the vector is used in the second function.

makeCacheinvatrix <- function(x = matrix()) {
  inv<- NULL
  set<-function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function() inv <<- solve(x)
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
    # If the inverse has already been calculatet, directly retrieve the inverse from the cache
  }
  data1 <- x$get()
  inv <- solve(data1, ...)
  x$setinv()
  inv        ## Otherwise, return a invatrix that is the inverse of 'x'
}


