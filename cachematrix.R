## These two functions is made to create, save, and call a 
## a matrix with inverse from cache

## The makeCacheMatrix will store the matrix in cache, 
## set an inverse matrix, get the inverse matrix
## and create list of functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  +     set <- function(y){
    +         x <<- y
    +         m <<- NULL
    +     }
  +     get <- function() x
  +     setInverse <- function(solve) m <<- solve
  +     getInverse <- function() m
  +     list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


## The cacheSolve calculates the invese matrix of the 
## makeCacheMatrix function.  A check is done to determine if
## calculation was done before.  If it was, it recalls the 
## information from the cache.  If not, it will calculate the 
## inverse matrix then save it in the cache

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  +     if(!is.null(m)){
    +         message("getting cached data")
    +         return(m)
    +     }
  +     data <- x$get()
  +     m <- solve(data,...)
  +     x$setInverse(m)
}
