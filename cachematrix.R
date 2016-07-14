## This code was created as part of programming assignment 2 in Coursera's "R Programming" course.
## Original code template was sourced from https://github.com/rdpeng/ProgrammingAssignment2

## makeCacheMatrix creates a matrix whose inverse will be solved then cached using cacheSolve.
    ## 4 functions (set, get, setinv, getinv) are defined in makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv 
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## cacheSolve checks if the inverse of the previous matrix is cached.
## If inv == NULL, then cacheSolve solves the matrix and caches its inverse.
## If inv != NULL, then cacheSolve returns the stored matrix inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

## IN ACTION

m <- matrix(c(1:4), 2, 2)
matrix_test <- makeCacheMatrix(m)
matrix_test$get() ## Returns "m" matrix.

cacheSolve(matrix_test) ## Solves "m" matrix and returns inverse.

## Both lines return previously solved inverse without recalculating if matrix_test is unchanged.
matrix_test$getinv() 
cacheSolve(matrix_test)