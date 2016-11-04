## Put comments here that give an overall description of what your
## functions do
## Description: We must develop two functions, one to cache the matrix and the other one
## to calculate the matrix inversion just in case it hasn't been calculated yet.
## Author: Alfonso Moralo
## Creation Date: 04/11/2016

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  ## x is a invertible matrix
  
  v_matrix <- NULL
  set <- function(y) {
    x <<- y
    v_matrix <<- NULL
  }
  
  get <- function() x
  setinv <- function(inverse) v_matrix <<- inverse
  getinv <- function() v_matrix
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  v_inverse = x$getinv()
  
  if(!is.null(v_inverse)){
    message("getting cached data")
    return(v_inverse)
  }
  
  ## if v_inverse is null (inverse is not calculated yet) then we calculate it
  data = x$get()
  v_inverse = solve(data,...)
  
  ## Assign the calculated value using set function
  x$setinv(v_inverse)
  
  return (v_inverse)
  
}

## To test the two previous functions...

random_data = rnorm(10000)
v_matrix = matrix(random_data, nrow=100, ncol=100)

data = makeCacheMatrix (v_matrix)
cacheSolve(data)
