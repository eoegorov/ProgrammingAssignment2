## This R script contains two functions. 
## The first one creates a special "matrix" object that can 
## cache its inverse.
## The second one computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse 
## has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

## the matrix should be a square one.

## This function stores matrix in a memory.

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y){
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(inv) inverse <<- inv
	getinverse <- function() inverse
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function would retrieve inverse of a matrix from memory, and if
## it does not exist, the function will calculate it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  inverse <- x$getinverse()
	  if (!is.null(inverse)){
	  message("getting cached data")
	  return(inverse)
	  }
	  data <- x$get()
	  inverse <- solve(data,...)
	  x$setinverse(inverse)
	  inverse
}
