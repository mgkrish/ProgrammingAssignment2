  ## This program is used to create and store matrix
  ## and caches its inverse.
  ## how to execute function
  ## Example : 
  ##i<- makeCacheMatrix(matrix(1:4,nrow=2,ncol=2)) 
  ##cacheSolve(i)
  ## Put comments here that give an overall description of what your
  ## functions do
  
  ## makeCacheMatrix function creates a matrix object
  ## The returned matrix object will contain a list of functions
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the matrix inverse
## 4.  get the value of the matrix inverse
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ##intialize variable
  m <- NULL
  
  
  set <- function(y) {
    x <<- y ## set the value to parent level
    m <<- NULL ## clear cached mean
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve function computes the inverse of matrix 
## and if not exists in cache it'll compute and  store 
## the inverse to object
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) { ## check for cached inverse matrix
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) ## computes inverse
  x$setinverse(m)
  m
  ## Return a matrix that is the inverse of 'x'
}
