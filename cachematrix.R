#
# Functions to create special "matrix", whihc caches inverse of the matrix 
# and avoids recomputing inverse of matrix if already has been computed
#


#
# Craetes a wrapper around matrix() object, which stores inverse along with matrix. 
# provedes get and set method to access the input matrix.
# provides getinv() and setinv() to access/set inverse of matrix.
# 
# parameters :
#             input matrix() 
# returns :
#             a list of these 4 methods
#
makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
 set <- function(y) {
   x <<- y
   inv <<- NULL
 }
 get <- function() x
 setinv <- function(inverse) inv <<- inverse
 getinv <- function() inv
 list(set=set,get=get,setinv=setinv,getinv=getinv)
}



#
# cached version of solve method, i.e. computes solve only if not has already been computed
# parameters :
#             a list return by makeCacheMatrix() and '...' which is extra argument list to actual solve(x,...)
# returns :
#             inverse of matrix cached or recently computed (if not in cache)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  inv
}
