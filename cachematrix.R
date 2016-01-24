## There are 2 functions:
## 1. makeCacheMatrix -- function to store matrix & its inverse
## 2. cacheSolve -- function to cache inverse matrix if available; otherwise, calculate it

##function to store matrix & its inverse
makeCacheMatrix <- function(x = matrix()) {
  #inverse matrix variable
  invMatrix <- NULL
  #store matrix
  set <- function(y = matrix()) {
    x <<- y
    invMatrix <<- NULL
  }
  #get stored matrix
  get <- function() x
  #store inverse matrix
  setInvMatrix <- function(y = matrix()) invMatrix <<- y
  #get stored inverse matrix
  getInvMatrix <- function() invMatrix
  #list of functions
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}

##function to cache inverse matrix if available; otherwise, calculate it
cacheSolve <- function(x = matrix(), ...) {
  #get inverse matrix
  invMatrix <- x$getInvMatrix()
  #if available, save it
  if(!is.null(invMatrix)) {
    message("getting cached data")
    return(invMatrix)
  }
  #otherwise, calculate it
  else {
    baseMatrix <- x$get()
    invMatrix <- solve(baseMatrix)
    x$setInvMatrix(invMatrix)
    return(invMatrix)
  }
}
