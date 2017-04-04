## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly The functions below cache the 
## inverse of a matrix.

## This first function creates a special "matrix", which is a list containing a function to:

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set=set, get=get,
       setinv = setinv,
       getinv = getinv)
}


## The following function calculates the inverse of the matrix created with the above 
## function. It takes the following steps: 

## 1. It first checks to see if the inverse of the matrix has already been calculated. 
## 2. If so, it gets the inverse of the matrix from the cache and skips the computation. 
## 3. Otherwise, it calculates the inverse of the data and sets the inverse matrix in the 
## cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
