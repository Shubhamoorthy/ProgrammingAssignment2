## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix used to cache the inverse and the Matrix itself using lexical scoping

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


## cacheSolve used to get inverse of the matrix from cache (lexical Scoping) if it exist in cache or creates one and stores in cache.

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
