## Caching the inverse of a Matrix
## functions do

## set the value of the matrix
## get the value of the matrix
## set the value of matrix inverse
## get the value of matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  mi=NULL 
  y<-NULL
  set <- function(y) {
    x <<- y
    mi <<- NULL
  }
  get <- function() x
  setinverse <- function(matrixinverse) mi <<- matrixinverse
  getinverse <- function() mi
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Functions checks if the inverse of matrix is already calculated
## It gets the inverse from the cache and skips calculation
## If inverse of matrix does not exist then Function calculates the inverse of matrix
## Sets the value of the matrix inverse in the cache by setinverse function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mi <- x$getinverse()
  if(!is.null(mi)) {
    message("getting cached data")
    return(mi)
  }
  data <- x$get()
  mi <- solve(data,...)
  x$setinverse(mi)
  mi
}
