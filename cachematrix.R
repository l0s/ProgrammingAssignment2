## makeCacheMatrix - create a matrix wrapper whose inverse is cacheable
## cacheSolve - invert a matrix; use cached result if available

## This method returns a wrapper for a matrix for which the inverse is
## cacheable
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function( y )
  {
    x <<- y
    inverse <<- NULL
  }
  get <- function()
  {
    x
  }
  setInverse <- function( i )
  {
    inverse <<- i
  }
  getInverse <- function()
  {
    inverse
  }
  list( set=set, get=get, setInverse=setInverse, getInverse=getInverse )
}

## This method calculates the inverse of matrix x
cacheSolve <- function(x, ...) {
  candidate <- x$getInverse()
  if( !is.null( candidate ) ) {
    candidate
  } else {
    inverse <- solve( x$get() )
    x$setInverse( inverse )
    inverse
  }
}
