##I assumed that the matrix supplied is always square and invertible (according to instructions)


##The function makeCacheMatrix  creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setIM <- function(matrix) m <<- matrix
  getIM <- function() m
  list(set = set, get = get,
       setIM = setIM,
       getIM = getIM)
}
##The function checks if the inverse has already been calculated, 
##then retrieve the inverse from the cache. Otherwise calculate it.
cacheSolve <- function(x, ...) {
  m <- x$getIM()
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  ##calculate only if it was not calculated before
  else {
  data <- x$get()
  m <- solve(data, ...)
  x$setIM(m)
  m
    }
}

##Check functions using multiplication of matrices
A<-matrix(c(15,0,-1,6), 2,2)
q<-makeCacheMatrix(A)
B<-cacheSolve(q)
B
A%*%B
matrix(1:4,2,2)
matrix(c(15,0,-1,6), 2,2)
matrix(c(1,0,0,0,1,0,0,0,1), 3,3)
