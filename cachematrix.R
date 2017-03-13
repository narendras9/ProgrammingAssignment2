## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setmatrix <- function(inverse) i <<- inverse
  getmatrix <- function() i
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getmatrix()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  
  ## Checks if it is possible to invert the matrix. Else sets the inverse back to Null
  if(!det(data)==0)
  {
    i <- solve(data)
  }
  else
  {
    i<-NULL
  }
  
  x$setmatrix(i)
  
  i
}
