## makeCacheMatrix is a function that creates a special "matrix"
## object that can cache its inverse

## If the matrix inverse has already been calculated, it will
## be finded in the cache and returned, if not it will be 
## calculate and saved in the cache.

makeCacheMatrix <- function(x = matrix()) {
  cache_x <- NULL # set the value of cache_x to NULL
  y <- NULL # set the value of y to NULL 
  set <- function(y) { # set the value of the matrix
    x <<- y # cache the inputted matrix
    cache_x <<- NULL # set the value of cached_x to NULL
  }
  get <- function() x # get the value of the matrix
  setinverse <- function(inverse) cache_x <<-inverse # set the value 
  getinverse <- function() cache_x # retrieve the value of the inverted matrix
  list(set = set, get = get, # create a list of the four functions
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve is a function that computes the inverse of the 
## special "matrix" returned by makeCacheMatrix


## If the cached inverse matrix is available, cacheSolve 
## will retrieve it, if not it will compute, cache and return it
cacheSolve <- function(x, ...) {
  ## Prints the matrix to be inverted
  message("Matrix to be inverted")
  print(x$get())
  message("")
  
  ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getinverse() # if an inverse has already calculate it gets it
  if (!is.null(inv_x)) { # check to find if cacheSolved has been run before
    message("Getting cached inverse matrix")
    return(inv_x)
  } 
  data <- x$get() # gets the value of the input matrix
  x$set <- data # sets the value of the input matrix
  inv_x <- solve(data) # invert the matrix
  ##  inv_x <- inverse(data, ...)
  x$setinverse(inv_x) # run the function to cache the inverted matrix
  message("Inverse matrix")
  return(inv_x)
}

## sample dato to test the function
##
## > m <- makeCacheMatrix()
## > m$set(matrix(c(3,2,4,3),2,2))
## > cacheSolve(m)
## Matrix to be inverted
## [,1] [,2]
## [1,]    3    4
## [2,]    2    3
##
## Inverse matrix
## [,1] [,2]
## [1,]    3   -4
## [2,]   -2    3
## > cacheSolve(m)
## Matrix to be inverted
## [,1] [,2]
## [1,]    3    4
## [2,]    2    3
## 
## Getting cached inverse matrix
## [,1] [,2]
## [1,]    3   -4
## [2,]   -2    3
##
## changing input data to show that function will not get then 
## from cache
##
## > m$set(matrix(c(3,2,5,6),2,2))
## > cacheSolve(m)
## Matrix to be inverted
## [,1] [,2]
## [1,]    3    5
## [2,]    2    6
## 
## Inverse matrix
## [,1]   [,2]
## [1,]  0.75 -0.625
## [2,] -0.25  0.375
## > cacheSolve(m)
## Matrix to be inverted
## [,1] [,2]
## [1,]    3    5
## [2,]    2    6
## 
## Getting cached inverse matrix
## [,1]   [,2]
## [1,]  0.75 -0.625
## [2,] -0.25  0.375
