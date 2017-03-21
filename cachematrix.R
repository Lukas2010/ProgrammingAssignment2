## Hello, i created two functions in which the inverse of a matrix is calculated
## when there is an existing inverse calculation of a matrix this one is retrieved
## thanks for reviewing it !

makeCacheMatrix <- function(x = numeric(), a = numeric(), b = numeric()) {
  s <- NULL
  set <- function(y, a = 1, b = 1) {
    x <<- y
    row_a <<- a
    col_b <<- b
    s <<- NULL
  }
  get <- function() matrix(x, a, b)
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The first functions main goal is to cache a matrix which is provided to this
## function. 
## I did not use x = matrix() and wrote every argument, while putting the matrix
## function at the get function

cacheinverse <- function(x, ...) {
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data,...)
  x$setinverse(s)
  s
}

## This function will calculate the inverse of a matrix upplied to it while
## checking beforehand whether the cache is filled yet with a if statement.