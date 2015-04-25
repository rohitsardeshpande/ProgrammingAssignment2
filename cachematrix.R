## Pair of functions to inverse a matrix, either using the solve() function on
## each run or retrieving a cached inverse of the matrix

## This returns a list of functions to manage the given matrix and its inverse.
## This can inverse a mtrix and cache it

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  
  set_matrix <- function (y) {
    x <<- y
  }
  
  get_matrix <- function() return(x)
  
  make_inverse <- function(x) {
    minv <<- solve(x)
  }
  
  get_inverse <- function() minv
  
  list(set_matrix = set_matrix, get_matrix = get_matrix,
       make_inverse = make_inverse, get_inverse = get_inverse)
}


## This utilises functions defined by makeCacheMatrix to inverse a given matrix
## and prints results

cacheSolve <- function(z, ...) {

  minv_local <- ""
  
  minv_cache <- z$get_inverse()
  
  if(!is.null(minv_cache)) {
    print("Inverse retrieved from cache")
    print(minv_cache)
  } else {
    m <- z$get_matrix()
    minv_local <- z$make_inverse(m)
    print("Inverse calculated")
    print(minv_local)
  }
}

## Simple function to test the above two functions

matrix_inverse_cache <- function() {
        
  x_matrix <- matrix(c(4, 7, 2, 6), 2, 2)
  f <- makeCacheMatrix(x_matrix)
  cacheSolve(f)
  cacheSolve(f)
}