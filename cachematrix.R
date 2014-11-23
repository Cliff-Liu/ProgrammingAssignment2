## Coursera, R Programming, Programming Assignment2
## Author: Cliff, 2014-11-23

## This R file contains two functions (makeCacheMatrix() and cacheSolve())
## which are used to caculate the inverse of input matrix. The functions
## can cache the cacualtion result, so if you try to get the same matrix' 
## inverse the 2nd time, the program can derive the cache result from previous
## caculation
## The precondition of the functions: the input matrix is an invertible one


## function makeCacheMatrix() returns a list of functions to cache
## the input matrix and it's inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  mm_origin   <- x
  mm_inverse  <- NULL
  
  set <- function(im_matrix){
    mm_origin <<- im_matrix
  }
  
  get <- function(){
    mm_origin
  }
  
  calcInverse <- function(){
    mm_inverse <- solve(mm_origin)
  }
  
  getInverse <- function(){
    mm_inverse
  }
  
  
  setInverse <- function(im_inverse){
    #message("set inverse matrix")
    mm_inverse <<- im_inverse
  }
  
  list(set          = set, 
       get          = get,
       calcInverse  = calcInverse,
       setInverse   = setInverse,
       getInverse   = getInverse)
}


## function cacheSolve() returns the inverse of matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of function 'x'
  
  mm_origin <- x$get()
  mm_inverse <- x$getInverse()
  if (is.null(mm_inverse)){
    
    # If the inverse has not been calculated before, caculate it
    # now, and save it into the cache
    # message("do matrix inverse cacluation using solve()")
    
    mm_inverse <- x$calcInverse()
    x$setInverse(mm_inverse)
  }else{
    # the inverse is retrived from cache
    # message("Inverse derived from cache")
  }
  mm_inverse
}
