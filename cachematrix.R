
## These functions provide for the ability to cache a matrix inverse 
## So that the overhead of calculating the inverse is done only once
## If the matrix is changed, the inverse will be recalculated when it is retrieved again. 


# This function creates a  special object which is capable of containing a matrix and
# the inverse of the matrix
# It also creates functions for getting and setting the matrix values and its inverse values  
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}




# This functions retrieves the inverse of a matrix if it has already been calculated
# It calculates the inverse and stores it if the matrix has been changed or the inverse
# has not yet been calculated
# It displays a message if it returns cached data and does not need to calculate the inverse  
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
		

