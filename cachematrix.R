
## The first function sets the value of the matrix, gets the value of the matrix, sets the value of the inverse matrix 
## and gets the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setImat <- function(solve) m <<- solve
  getImat <- function() m
  list(set = set, get = get,
       setImat = setImat,
       getImat = getImat)
}



## The second function checks to see if the matrix inverse has already been calculated, if not it does so 
## and returns the matric inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getImat()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setImat(m)
  m
}
