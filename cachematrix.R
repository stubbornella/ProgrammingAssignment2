## Cache Matrix

## Calculating the inverse of a matrix is sometimes expensive.
## This file creates a cache matrix object that stores it's inverse
## so it does not need to be recalculated. 

## makeCacheMatrix  
## creates a cached matrix object
## params - x: matrix you want to convert to a cache matrix, must be invertable

## Methods
## setinverse - stores the inverse, parameter inverse must be a matrix
## getinverse - returns the inverse of the matrix, 
##              returns null if the inverse hasn't been calculated 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(mat) {
    x <<- mat
    inv <<- NULL
  }
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve - calculates the inverse of a CacheMatrix object and stores it on that object
## params - x - a CacheMatrix object
## returns - the inverse of the matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  inverse <- solve(x$get())
  x$setinverse(inverse)
  inverse
}

## Usage
## https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md#putting-the-pieces-together-how-the-functions-work-at-runtime