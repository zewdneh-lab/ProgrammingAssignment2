## The following 'makeCacheMatrix' is a function that creates a  special "matrix" object which will cache its inverse for the input (an invertible matrix)

makeCacheMatrix <- function(x = matrix()) {
  invrt <- NULL
  set <- function(y) {
    x <<- y
    invrt <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invrt <<- inverse
  getInverse <- function() invrt
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## The following 'cacheSolve' is a function which computes the inverse of the special "matrix" makeCacheMatrix above. 
##If the inverse was calculated above and the matrix has not been changed, then the cacheSolve should retrieve the inverse  from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invrt <- x$getInverse()
  if (!is.null(invrt)) {
    message("getting cached data")
    return(invrt)
  }
  data <- x$get()
  invrt <- solve(data, ...)
  x$setInverse(invrt)
  invrt
}
