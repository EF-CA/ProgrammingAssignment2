## Put comments here that give an overall description of what your
## functions do


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      # set inv ("inverse") to NULL -- clear out any previous data
      inv <- NULL
      
      # set function assigns the argument of the function to x
      # also resets inverse to NULL 
      set <- function(y) {
            x<<- y
            inv<<- NULL
      }
      
      # get function returns the matrix created with set
      get <- function() x
      
      #setinv function saves value of inverse created by cacheSolve
      setinv <- function(solveinv) inv <<- solveinv

      #getinv function returns value of inverse
      getinv <- function() inv
      
      #create a list of these functions
      list(set=set,get=get,setinv=setinv,getinv=getinv)
      
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x) {
      # gets current value of inverse
      inv <- x$getinv()
      # checks is value is null or not and returns val if exists
      if (!is.null(inv)){
            message('getting cached data')
            return(inv)
      }
      # else calc and return new inverse val
      
      solveinv <- solve(x$get())
      x$setinv(solveinv)
      solveinv
      
}

#test functions
M <- makeCacheMatrix()
M$get()
M$set(matrix(c(2,2,3,2),2,2))
M$get()
M$getinv()
cacheSolve(M)
cacheSolve(M)
M$getinv()
