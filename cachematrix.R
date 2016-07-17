## Functions in ths R file can compute the inverse of a square matrix and caching the result.

## Write a short comment describing this function
## makeCacheMatrix creates a special "vector", which is really a list containing a function to
## 1. set the value of the square matrix
## 2. get the value of the square matrix
## 3. set the value of the inverse square matrix
## 4. get the value of the inverse square matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinversion <- function(solve) m <<- solve
    getinversion <- function() m
    list(set = set, get = get,
         setinversion  = setinversion ,
         getinversion  = getinversion )
}


## cacheSolve calculates the inverse of a square matrix created with the function makeCacheMatrix. 
## It first checks if the inverse has already been calculated. If so, it gets the inverse of a square matrix
## from the cache and skips the computation. Otherwise, it calculates the inverse and sets the value
## of the inverse of a square matrix in the cache via the setmean function.
cacheSolve <- function(x, ...) {
    m <- x$getinversion()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinversion(m)
    m
          # Return a matrix that is the inverse of 'x'
}
