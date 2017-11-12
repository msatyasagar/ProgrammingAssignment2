## makeCacheMatrix: This function will create special "matrix" object that can cache the inverse of Matrix.

makeCacheMatrix <- function(x = matrix()) {

  matr <- NULL
  set <- function(y) {
    x <<- y
    matr <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) matr <<- inverse
  getinverse <- function() matr
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##cacheSolve: This function will compute inverse of special matrix returned by makeCacheMatrix function.

cacheSolve <- function(x, ...) {
  mat <- x$getinverse()
  if (!is.null(mat)) {
    message("Here is the cached data")
    return(mat)
  }
  data <- x$get()
  mat <- solve(data, ...)
  x$setinverse(mat)
  mat
}
