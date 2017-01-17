## This function is split into two major parts, MakeCacheMatrix and CacheSolve. CacheSolve solves the inverse of a matrix but first checks
##to see if the inverse was already solved. MakeCacheMatrix saves the inverses of matrices already solved.

## Objective of this function is to "save" the inverse of matricies already solved inorder to save processing time

MakeCacheMatrix <- function(x = matrix()) {##took the Vector function of this and made some minor changes
  a <- NULL
  set <- function(y) {
    x <<- y
    a <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) a <<- solve
  getinverse <- function() a
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## Objective of this function is to return the inverse of a matrix. First it sees if the matrix inverse has been already solved and if not solves it
CacheSolve <- function(x, ...) {##once again took the vector function of this and made some minor changes
  a <- x$getinverse()
  if(!is.null(a)) {
    return(a)
  }
  data <- x$get()
  a <- solve(data, ...)
  x$setinverse(a)
  a
}
