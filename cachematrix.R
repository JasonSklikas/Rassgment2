##Computes an inverse matrix but if the matrix inversion already exist
##from a previews callculation it recals it to avoid further computations

## Creates a special Matrix that:
##set the matrix and gets the matrix
##set the inverse kai gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inver<-NULL
  set<- function(y) {
    x<<- y
    inver<-NULL
  }
  get<- function()x
  set_inverse <- function(inverse) inver<<- inverse
  get_inverse<- function() inver
  list(set=set, get=get, set_inverse=set_inverse,
       get_inverse=get_inverse)
}

## Calculates the inverse given matrix but if it has already
## benn calculated it retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  inver<- x$get_inverse()
  if (!is.null(inver)){
      message("Getting cache data")
    return(inver)
  }
  data<- x$get()
  inver<- solve(data, ...)
  x$set_inverse
  inver
}

