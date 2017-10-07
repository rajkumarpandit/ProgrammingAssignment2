## Programming Assignment 2 - R Programming
## Data Science Specialization Track
##
## The two functions shown here are to enable caching of
## inverse of a matrix which is otherwise a resource consuming process
## especially matrix size is big.
## Computing the inverse again if it is already computed, 
## doesn't make any sense as we can fetch the pre-computed result.
##
## ---------------------------------------------------
## Function makeCacheMatrix:
## The input into this function is a matrix
## and returns a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix
##------------------------------------------
## Usage :
## x <- matrix(1:4, nrow=2, ncol=2)
## m <- makeCacheMatrix(x)

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv_matrix <<- inverse
  getinverse <- function() inv_matrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
  inv_matrix <- x$getinverse()
  if(!is.null(inv_matrix)) {
    message("Getting data from the cache ....")
    return(inv_matrix)
  }
  data <- x$get()
  inv_matrix <- solve(data)
  x$setinverse(inv_matrix)
  inv_matrix
}
# executing the functions
#
# x <- matrix(1:4, nrow=2, ncol=2)
# source("cachematrix.R")
# > x
#       [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > m<-makeCacheMatrix(x)
# > m
# $set
# function (y) 
# {
# x <<- y
# inv_matrix <<- NULL
# }
# <environment: 0x0000000013d9df00>
#   
# $get
# function () 
# x
# <environment: 0x0000000013d9df00>
#   
# $setinverse
# function (inverse) 
#   inv_matrix <<- inverse
# <environment: 0x0000000013d9df00>
# 
#   $getinverse
# function ()
#   inv_matrix
# <environment: 0x0000000013d9df00>
# 
# > m$get()
#       [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(m)
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > ##no Cache in the first run
# > cacheSolve(m)
# Getting data cache ....
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(m)
# Getting data cache ....
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# >
