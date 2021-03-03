## Cache the inverse of a Matrix or calculate it if it hasn't been cached

## |Creates an object makeCacheMatrix that contains the functions set, get, setinv, and getinv

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) { #creates the set function
    x <<- y
    i <<- NULL
  }
  get <- function() x #creates the get function
  setinv <- function(inv) i <<- inv #creates the setinv function
  getinv <- function() i #creates the getinv function
  list(set = set, get = get, #returns the list of functions
       setinv = setinv,
       getinv = getinv)
}


## checks cache to see if the inverse is already stored, calculates it if isn't

cacheSolve  <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) { #checks to see if the inverse is already computed
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- inv(data, ...) #computes inverse
  x$setinv(i)
  i
}