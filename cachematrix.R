## Put comments here that give an overall description of what your
## functions do

## Create "special matrix"

makeCacheMatrix <- function(x = matrix()) {
      
      # Inverted matrix initally set to NULL
      inv_matrix <- NULL
      
      # Set the matrix
      set <- function(y) {
            x <<- y
            inv_matrix <<- NULL
      }
      
      # Get the matrix
      get <- function() x
      
      # Set the inverted matrix
      set_inv <- function(solve) inv_matrix <<- solve
      
      # Get the inverted matrix
      get_inv <- function() inv_matrix
      
      
      list(set = set,
             get = get,
             set_inv = set_inv,
             get_inv = get_inv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      inv_matrix <- x$get_inv
      if (!is.null(inv_matrix)) {
            message("Retrieving cached inverted matrix")
            return(inv_matrix)
      }
      
      matrix <- x$get()
      
      inv_matrix <- solve(matrix)
      
      x$set_inv(inv_matrix)
      
      return(inv_matrix)
}

