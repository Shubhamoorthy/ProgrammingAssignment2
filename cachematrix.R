## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  inv.cache <- NULL
  set <- function(y) {
    x <<- y
    inv.cache <<- NULL
  }
  get <- function() x
  setinv <- function(inverse.mat) inv.cache <<- inverse.mat
  getinv <- function() inv.cache
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv.req <- x$getinv()
  if(!is.null(inv.req)) {
    message("getting cached data")
    return(inv.req)
  }
  data <- x$get()
  inv.req <- solve(data, ...)
  x$setinv(inv.req)
  inv.req
  
}
