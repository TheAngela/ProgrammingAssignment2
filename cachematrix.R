##These functions compute and store the inverse of a matrix.

##Good ways to use these functions include the following commands:
##myMatrix = makeCacheMatrix(<yourmatrix here>)
##myMatrix$get()         # Returns original matrix
##cacheSolve(myMatrix)   # Computes, caches, and returns matrix inverse
##myMatrix$getinverse()  # Returns matrix inverse
##cacheSolve(myMatrix)   # Returns cached matrix inverse using previously computed matrix inverse


## The function below creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


##This function computes the inverse of the special matrix returned by makeCacheMatrix above
##If the inverse has already been calculated (and the matrix has not changed),
##then the cacheSolve should retrieve the inverse from the cache without recomputing the inverse

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}