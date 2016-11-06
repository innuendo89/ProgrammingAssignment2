## In order to avoid the often time-consuming re-computation of matrix inversion, it is reasonable to create
## functions, that can store the matrix and its cached inverse and retrieve the already calculated inversion 
## anytime from cache and without additional computations.

## Using the makeCacheMatrix function, a matrix object is constructed and its inverse is stored in cache.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  set_inv <- function(inverse) inv <<- inverse
  get_inv <- function() inv
  
  list(set = set,
       get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}

## The cacheSolve function is able to compute the inverse of the matrix created with the makeCacheMatrix function. 
## In case the inversion of a matrix was already calculated and the matrix input was not modified, the inverse will 
## be retrieved from cache.

cacheSolve <- function(x, ...) {
  inv <- x$get_inv()
  if(!is.null(inv)) {
    message("retrieving cached data")
    return(inv)
  }
  new <- x$get()
  inv <- solve(new) ## getting inverse
  x$set_inv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}

## Testing
new_matrix <- makeCacheMatrix(matrix(rnorm(9),3,3))
new_matrix$get()
new_matrix$get_inv()
cacheSolve(new_matrix)
new_matrix$get_inv()

matrix_new <- makeCacheMatrix(matrix(rnorm(25),5,5))
matrix_new$get()
matrix_new$get_inv()
cacheSolve(matrix_new)
matrix_new$get_inv()
