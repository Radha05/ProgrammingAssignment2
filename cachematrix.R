## The functions makeCacheMatrix() and cacheSolve() are used to cache the inverse of a
## matrix instead of computing the inverse repeatedly, as Matrix inversion computation is 
## costly computation usually.

## The following function creates a special matrix object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
       inver <- NULL
       set <- function(y) {
       x <<- y
       inver <<- NULL
  }
       get <- function() x
       setinver <- function(inverse)  inver <<- inverse
       getinver <- function() inver
    list(set = set, get = get,setinver = setinver,getinver =getinver)
  }
  
## The following function computes the inverse of the special matrix 
##returned by makeCacheMatrix. This function will retrieve the inverse 
##from the cache if it has already been created.


cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
       inver <- x$getinver()
       if(!is.null(inver)) {
       message("getting cached data")
       return(inver)
       }
       data <- x$get()
       inver <- solve(data, ...)
       x$setinver(inver)
       inver
  }
