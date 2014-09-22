## These functions are part of programming assignment 2 to work with
## elements cached in memory

## This function is used to cache a 
## matrix and its inverse it returns
## a vector of functions that can be used to
## manipulate the cached matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ix <- NULL
  
  
  set <- function(y){
    x <<- y
    ix <<-  NULL
    
  }
  
  get <- function() x
  
  setinverse <- function(inv) ix <<- inv
  getinverse<- function() ix

  list (set = set, get = get,
        setinverse = setinverse, getinverse = getinverse)
}


## This function returns the cached matrix inverse of the matrix 
## created in the makeCacheMatrix function, if the cached inverse 
## is not present then it solves and creates one

cacheSolve <- function(x, ...) {
    
   m <- x$getinverse()
   if(!is.null(m)){
     message("getting cached data - Im fast !!")
     return (m)
   }
   
   im <- solve(x$get())
   x$setinverse(im)
   
   im
   
}
