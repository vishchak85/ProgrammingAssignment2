##Two functions are used to complete this project, a matrix and cache. 
##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to:
##set the value of the matrix, get the value of the matrix, set the value of the inverse & get the value of the inverse

## This Function creates special matrix as stated above.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## cacheSolve should retrieve the inverse from the cache if the inverse has already been calculated (where matrix remain unchanged), 


cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

##Testing:

## We are assuming that matrix is always invertible.
Z <- matrix(c(1,2,3,4),2,2)
#solve(Z) #We pretend that this cant't happen xD
L <- makeCacheMatrix(Z)  ## This will create a special matrix

cacheSolve(L) #inverse returned after computation