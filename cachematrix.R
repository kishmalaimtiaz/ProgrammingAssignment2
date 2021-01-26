##Functions are set of codes that can be called to do a task multiple times

##The makeCacheMatrix creates a special matric which sets and and gets the
##value of matrix and inverse respectively
library(MASS) ## used yo compute inverse for non-square matrixes also

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ##inv is a matrix initialized to NULL
  set <- function (y){
    x <<- y
    inv <<- NULL
  }
  get <-function()x
  setinv <- function(inverse)inv <<- inverse
  getinv <- function(){
    inver <- ginv(x)
    inver%*%x
    ## the above is to obtain inverse of a matrix
  }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## cacheSolve calculates the inverse of matrix created using above ftn

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return (inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
       
}
