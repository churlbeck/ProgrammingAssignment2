## The makeCacheMatrix function accepts a matrix as input and produces an object
## consisting of the matrix and several functions for managing it:
##  x$set(data) - stores the value of the original matrix passed to makeCacheMatrix
##  x$get() - retrieves the value of the original matrix passed to makeCacheMatrix
##  x$set_cache(data) - stores the value "data" to the cache
##  x$get_cache() - retrieves the value of the data stored in the cache

## The output object can be passed to another function, cacheSolve, to produce the
## inverse of the matrix.  cacheSolve will not accept a matrix directly; it uses
## the makeCacheMatrix's management functions to store the inverse in cache

## Usage:
##  m <- matrix(rnorm(9), 3)  # create a matrix m
##  x <- makeCacheMatrix(m)   # create a makeCacheMatrix object x based on m
##  cacheSolve(x)             # invert matrix m via x
##  cacheSolve(x)             # subsequent calls to x retrieve the solution from cache

makeCacheMatrix <- function(x = matrix())
  {
  # Initialize the cache
  cache_data <- NULL
  
  # This function stores the input matrix and re-initializes the cache
  set <- function(y)
    {
    x <<- y
    cache_data <<- NULL
    }
  
  # This function returns the value of the input matrix x
  get <- function() x
  
  # This function stores an arbitrary piece of data in the cache
  set_cache <- function(data) cache_data <<- data
  
  # This function retrieves the data stored in the cache
  get_cache <- function() cache_data
  
  # Return a list consisting of the functions defined above
  list(set = set, get = get,
       set_cache = set_cache,
       get_cache = get_cache)
  }


## This function returns the inverse of a matrix.  It expects a makeCacheMatrix
## object x as input, exploiting its caching function to store the results of the
## calculation and optimize subsequent retrievals

cacheSolve <- function(x, ...)
  {
  # Attempt to retrieve the inverse of the matrix (if already cached)
  inverse_matrix <- x$get_cache()
  
  # If the cache is empty, then calculate the inverse of the matrix stored in object "x"
  if(is.null(inverse_matrix))
    {
    # Retrieve the original matrix
    source_matrix <- x$get()
  
    # Calculate the inverse of the original matrix using the "solve" function
    inverse_matrix <- solve(source_matrix, ...)

    # Cache the results of the "solve" function back in object "x" for future invocations
    x$set_cache(inverse_matrix)
    }
  
  # If the cache is already populated, then acknowledge it with a message
  else
    {
    message("inverse matrix retrieved from cache...")
    }

  # Return inverse of the original matrix
  inverse_matrix
  }
