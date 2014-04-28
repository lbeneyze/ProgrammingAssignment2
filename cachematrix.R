##========================================================
##makeCacheMatrix
##========================================================
## This function creates a special "matrix" object that can cache its inverse.
## It has 4 functions, declare in a list of attributes:
##   1 set(): the matrix can be set to the cache
##   2 get(): the matrix can be get from the cache
##   3 setinverse(): the inverse of the matrix set to the cache
##   4 getinverse(): the inverse of the matrix can be get from the cache
##
##========================================================
makeCacheMatrix <- function(x = matrix()) {

  ## initialize a variable
  m <- NULL
  
  ## create a set function that takes a y argument
  ## y is set to x
  ## null is set to m
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## create a get function of the matrix
  get <- function() x
  
  ## declare setinverse() function
  ## set the inverse of the matrix in the cache
  setinverse <- function(inverse) m <<- inverse
  
  ## declare getinverse() function
  ## get the inverse of the matrix from the cache
  getinverse <- function() m
  
  ## declare a list with the functions of the special matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

##========================================================
##cacheSolve
##========================================================
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed)
##, then the cachesolve should retrieve the inverse from the cache.
##========================================================

cacheSolve <- function(x, ...) {
  
  ## get the inversie of the matrix x from the cache
  m <- x$getinverse()
  
  ## check if there is an inverse of the matric in the cachec
  if(!is.null(m)) {
    ## if inverse in the cache then retrieve from the cache 
    ## indicate the user that he is retrieving information from the cache
    message("getting cached inverse matrix")
    return(m)
  }
    
  ## if no inverse in the cache then get the matrix from the cache 
  data <- x$get()
  ## calculate the inverse of the matrix
  m <- solve(data)%*%data
  ## set the inverse of the matrix in the cache
  x$setinverse(m)
  
  ## Return a matrix that is the inverse of 'x'
  m
  
}






