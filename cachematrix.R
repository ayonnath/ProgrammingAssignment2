## Pair of functions that cache the inverse of a matrix

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## initialize the value of the matrix inverse to NULL
  inversematrix <- NULL
  set <- function(y) {                      
    x <<- y
    inversematrix <<- NULL              
  }
  get = function() x
  ## Method to set the inverse of the matrix
  setinverse = function(inverse) inversematrix <<- inverse
  ## Method to get the inverse of the matrix
  getinverse = function() inversematrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## `cacheSolve` function computes the inverse of the special "matrix" returned by 
##`makeCacheMatrix` above
cacheSolve <- function(x, ...) {
  inversematrix <- x$getinverse()
  ##Only caclulate inverse if matrix has changed or it hasn't been calculated yet
  if(!is.null(inversematrix)) {
    message("Getting cached inverse matrix")
    return(inversematrix)
  }
  ## Get the matrix from object
  matrixtoinverse <- x$get()
  inversematrix <- solve(matrixtoinverse)
  ## sets the value of the inverse in the cache via the setinverse function.
  x$setinverse(inversematrix)
  inversematrix
}