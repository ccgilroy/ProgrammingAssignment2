## Together, these functions solve a square matrix,
## and store the resulting inverse for reuse to 
## avoid recalculating it

## Stores a matrix and its inverse as a list of
## accessor functions which can assign those values 
## and return them

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## Solves a matrix provided in the cached form
## from above, first checking to see if the 
## inverse is present in the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv  
}
