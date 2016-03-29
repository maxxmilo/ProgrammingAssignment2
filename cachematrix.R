## makeCachMatrix makes a "special matrix" and caches its inverse
## If already cached, cacheSolve returns the inverse of the matrix
## If not already cached, cacheSolve computes the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      
      # Inverse of the matrix initally set to NULL
      inv_matrix <- NULL
      
      # Set the matrix and reset the inverse of the matrix to null
      set <- function(y) {
            x <<- y
            inv_matrix <<- NULL
      }
      
      # Get the matrix
      get <- function() x
      
      # Set the inverse of the matrix
      set_inv <- function(solve) inv_matrix <<- solve
      
      # Get the inverse of the matrix
      get_inv <- function() inv_matrix
      
      # Create "special" matrix list
      list(set = set,
             get = get,
             set_inv = set_inv,
             get_inv = get_inv)
}


## If already cached, cacheSolve returns the inverted matrix
## If not already cached, cacheSolve computes the inverse of the matrix

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      
      # Set inv_matrix using get_inv
      inv_matrix <- x$get_inv
      
      # If inv_matrix has already been cached (is null), return it
      if (!is.null(inv_matrix)) {
            # Output a nice message so folks know what's goin down
            message("Retrieving cached inverted matrix")
            return(inv_matrix)
      }
      
      # Else if the inverse matrix has not yet been cached:
      
      # Get the matrix
      matrix <- x$get()
      
      # Compute the inverse
      inv_matrix <- solve(matrix, ...)
      
      # Set the inverse
      x$set_inv(inv_matrix)
      
      # Return the inverse
      return(inv_matrix)
}

