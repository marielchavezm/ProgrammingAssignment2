## the functions below, are useful to avoid the recalculation by storing data in cache.
## Write a short comment describing this function
## The makeCacheMatrix function creates an object matrix thah stores the matrix and inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
   inv <<- NULL
  }
  get <- function() x
  setinv <- function(inversa) inv <<- inversa
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}



## cacheSolve  this function computes the function solve in order to get the inverse.
## If the inverse already exists the function retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}


mat <- matrix(rnorm(16),4,4)
mat1 <- makeCacheMatrix(mat)
mat1$get()
cacheSolve(mat1)
