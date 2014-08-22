## Put comments here that give an overall description of what your
## functions do


# Usage:
#   mat <- matrix(rnorm(9), nrow = 3)  
#   mat
#   cached_mat <- makeCacheMatrix(mat)  
#   cached_mat$get()
#   cacheSolve(cached_mat) 
#   cacheSolve(cached_mat)  
#    

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## set the value of the matrix
  inv<- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## get the value of the matrix
  get <- function() x
  
  ## set the inverse of the matrix
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  
  ## get the inverse of the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {## Return a matrix that is the inverse of 'x'
  
  ## get the inverse of the matrix        
  inv <- x$getinverse()
  
  ## check if it is cached  
  if(!is.null(inv)) {
    message("<<getting cached inverse matrix>>")
    return(inv)
  }
  ## solve the inverse of the matrix   
  data <- x$get()
  inv <- solve(data, ...)
  ## set the inverse of the matrix 
  x$setinverse(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
  
}
