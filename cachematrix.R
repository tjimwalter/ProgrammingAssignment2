## This file contains functions to implement the cachematrix Problem
## At the bottom of the file are test cases
## if you source the complete file the test cases execute


## Creates an object containing 2 pieces of data and 4 functions
##    m_matrix  : the incoming matrix (x) is stored in this member variable
##    m_inverse : the inverse of the matrix is stored in this member variable
##    setMatrix : resets the value of m_matrix
##              : NB, m_matrix is set at construction time
##    getMatrix : returns m_matrix
##    setInverse: sets the value of m_inverse
##    getInverse: returns the value of m_inverse
makeCacheMatrix <- function(m = matrix()) {
  m_matrix  <- m
  m_inverse <- NULL
  
  setMatrix <- function(m)   { m_matrix <<- m; m_inverse <<- NULL}
  getMatrix <- function()    { return(m_matrix)                  } 
  setInverse<- function(inv) { m_inverse <<- inv                 }
  getInverse<- function()    { return(m_inverse)                 }
  
  return(list(setMatrix  = setMatrix, 
              getMatrix  = getMatrix,
              setInverse = setInverse,
              getInverse = getInverse))
}


## Takes a CacheMatrix object as input and returns the inverse as a matrix
cacheSolve <- function(m, ...) {
  inv <- m$getInverse()
  if (!is.null(inv)){
    message("using cache")
    return(inv)
  }
  
  return(m$setInverse(solve(m$getMatrix())))
}

#################### test cases ##################
m <- matrix(c(rep(1:5,3),1), 4, 4)      # create an invertible matrix 
cm <- makeCacheMatrix(m)                # create a cacheMatrix from m
print(cacheSolve(cm))                   # solve, cache inverse, return inverse
print(cacheSolve(cm))                   # return the inverse using the cache


m2 <- matrix(c(rep(2:6,3),1), 4, 4)     # create an invertible matrix 
cm2 <- makeCacheMatrix(m2)              # create a cacheMatrix from m
print(cacheSolve(cm2))                  # solve, cache inverse, return inverse
print(cacheSolve(cm2))                  # return the inverse using the cache



