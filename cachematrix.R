
make_cache_matrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse variable as NULL

  set <- function(y) {  # Reset the cached inverse
    x <<- y
    inv <<- NULL
  }

  get <- function() x  # Retrieve the matrix

  set_inverse <- function(inverse) inv <<- inverse  # Cache the inverse

  get_inverse <- function() inv  # Retrieve the cached inverse

  list(
    set = set,
    get = get,
    set_inverse = set_inverse,
    get_inverse = get_inverse
  )
}

cache_solve <- function(x, ...) {
  inv <- x$get_inverse()  # Retrieve the cached inverse, if available

  if (!is.null(inv)) {  # Check if the inverse is already cached
    message("getting cached data")  # Print a message if cached data is used
    return(inv)  # Return the cached inverse
  }

  mat <- x$get()  # Retrieve the matrix
  inv <- solve(mat, ...)  # Compute the inverse
  x$set_inverse(inv)  # Cache the computed inverse
  inv  # Return the inverse
}