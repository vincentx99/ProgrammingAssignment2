##These functions serve the purpose of caching an inputted matrix as well as its inverse


## Stores the values of the matrix 

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)

}


##Calculates the inverse of the matrix assuming the value isn't already cached
##otherwise the cached value is returned. 

cacheSolve <- function(x, ...) {
  
  inv <- x$getInv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
