## Put comments here that give an overall description of what your
## functions do

## The goal of this assignment is to create a pair of functions called "makeCacheMatrix"
## and "cachesolve" that can create and cache the inverse of a given matrix.

## Write a short comment describing this function

## makeCacheMatrix is a function designed to create a special matrix object,
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
## This line above sets inv to NULL any time makeCacheMatrix is run and overwrites
## any previous inv value that had been cached prior to this function's running.
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}



## Write a short comment describing this function

## cacheSolve is a function that computes the inverse of the matrix returned
## by makeCacheMatrix. However, if the inverse has already been calculated, and
## we haven't changed the matrix, then cacheSolve should retrieve that inverse
## from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix, that is the inverse of the x from the previous function
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached information")
    return(inv)
    ## the portion above asks whether or not inv has a cached value other than NULL
    ## If it does, the console will print "Getting cached information" before
    ## returning the cached value.
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}