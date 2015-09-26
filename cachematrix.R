## The following pair of functions ("makeCacheMatrix" and "cacheSolve") caches 
## the invese of a matrix specified by user.
## This pair of function is one of the solutions which improves times and 
## the cost of the hardware used for matrix inversion.
## NB: This code works only if matrix is square and invertible.

## The "makeChacheMatrix" function stores the inverse of a matrix created 
## in the solve function.
## This also returns a list of function used in the "cacheSolve" function.

makeCacheMatrix <- function(x = matrix()) {
  ## Make the cached inverse matrix NULL to initialise  
  cachemx <- NULL
  
  ## Set a matrix in the working environment
  set <-function (y) {
    x<<-y
    cachemx <<- NULL
  }
  
  ## Get the matrix value
  get <- function () x
  ##  Set the inverse of the matrix and stores in cache
  setInverse <-function(inverse) cachemx <<-inverse
  ## Get the inverse of the matrix
  getInverse <-function () cachemx
  
  ## Return the above functions to the working environment 
  list (set = set, get = get, 
        setInverse = setInverse,
        getInverse = getInverse)
}


## The "cacheSolve" function computes the invese of the matrix set 
## in the "makeCacheMatrix" function above. 
## Unless the matrix is a new matrix, this function retrieves
## the cached matrix. 

cacheSolve <- function(x, ...) {
  
  ## Get the inversed matrix saved in cache
  cachemx <- x$getInverse()
  
  ## If the inverted matrix exists, 
  ## display a message ("getting cached data"), then 
  ## get the inverted matrix from cache 
  
  if (!is.null(cachemx)) {
    message ("getting cached data")
    return (cachemx)
    }
  
  ## If not, get the matrix in the working environment
  matrix <-x$get()
  ## then create the invese matrix and save it in cache
  cachemx <-solve(matrix, ...)
  x$setInverse(cachemx)
  cachemx
 }
