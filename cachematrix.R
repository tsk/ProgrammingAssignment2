## Functions to generate a cached matrix of x with the cached computation
## of the iverse of x

## Usage:
## > m1 <- matrix (data = c(2,2,3,2), nrow = 2, ncol =2)
## > m2 <- makeCacheMatrix(m1)
## > cacheSolve(m2)
## [,1] [,2]
## [1,]   -1  1.5
## [2,]    1 -1.0
## > cacheSolve(m2)
## getting cached data
## [,1] [,2]
## [1,]   -1  1.5
## [2,]    1 -1.0
## >

## makeCacheMatrix
## input:
## x -> a matrix
## methods:
## get() -> return the matrix x
## set(y) -> set x to y
## setinv(inverse) -> set the inverse of the matrix
## getinv() -> return the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <-function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set,get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve: calculate and return the inverse of the cacheMatrix
## input:
## x -> cacheMatrix
## return:
## The inverse of the cacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  A <- x$get()
  inv <- solve(A)
  x$setinv(inv)
  inv
}
