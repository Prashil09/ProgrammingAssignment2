# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(m = matrix()) 
{
  inverse <- NULL
  set <- function(z) 
    {
      m <<- z
      inverse <<- NULL
    }
  get <- function() m
  Setinv <- function(inv) inverse <<- inv
  Getinv <- function() inverse
  list(set = set,
       get = get,
       Setinv = Setinv,
       Getinv = Getinv)
}


# This function computes the inverse of the special "matrix" created by makeCacheMatrix above.

cacheSolve <- function(m, ...) 
{
  inverse <- m$Getinv()
  if (!is.null(inverse)) 
    {
      message("getting cached data")
      return(inverse)
    }
  mat <- m$get()
  inverse <- solve(mat, ...)
  m$Setinv(inverse)
  inverse
}