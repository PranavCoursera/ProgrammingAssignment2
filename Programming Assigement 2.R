## Put comments here that give an overall description of what your
## functions do

## Objective of this function is to "save" the inverse of matricies already solved inorder to save processing time

makeCacheMatrix <- function(x = matrix()) {##took the Vector function of this and made some minor changes
  a <- NULL
  set <- function(y) {
    x <<- y
    a <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() a
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## Objective of this function is to return the inverse of a matrix. First it sees if the matrix inverse has been already solved and if not solves it

cacheSolve <- function(x, ...) {##once again took the vector function of this and made some minor changes
  a <- x$getinverse()
  if(!is.null(a)) {
    return(a)
  }
  data <- x$get()
  a <- solve(data, ...)
  x$setinverse(a)
  a
}