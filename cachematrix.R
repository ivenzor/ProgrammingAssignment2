## The following functions will be used to compute
## the inverse of a given square matrix.
## The inverse matrix will be cached, so next time we want
## to get the inverse matrix for the same given matrix
## it won't be computed again.


## Similar to the vector mean example, 
## makeCacheMatrix will be called to create the special object.
## For example:  test <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL			                  ## "i" will store the cached value.
  set <- function(y) {
      x <<- y			                  ## our matrix.
      i <<- NULL
  }
  get <- function() x		          ## returns the matrix.
  ##
  ## here the inverse value is cached with the << operator.
  setinverse <- function(inverse) i <<- inverse	
  ##
  getinverse <- function() i	    ## returns the cached value, if any.
  list(set = set, get = get,      ## our working functions.
       setinverse = setinverse,
       getinverse = getinverse)
}


## Similar to de vector mean example, 
## cacheSolve will be called to compute the inverse matrix 
## or to retrieve the cached value.
## The makeCacheMatrix function should have been called before cacheSolve. 
## Then, we call cacheSolve to get the inverse: cacheSolve(test)
## If we call cacheSolve(test) again, we should get the "getting cached data" 
## message because it will be using the cached value.

cacheSolve <- function(x, ...) {
  ##
  ## First we check if the inverse value has already been computed.
  ## If "i" is null, the inverse value has not already been computed.
  ## If "i" is not null the inverse value has already been computed.
  i <- x$getinverse()					
  if(!is.null(i)) {
    message("getting cached data")			
    return(i)		                  ## return the cached value.
  }
  ##
  ## If "i" is null we need to fetch the matrix to compute the inverse.
  data <- x$get()	
  ##					
  i <- solve(data, ...)		        ## compute the inverse using solve.
  x$setinverse(i)			            ## after computed, it will be cached.
  i				                        ## the inverse is returned.
}
