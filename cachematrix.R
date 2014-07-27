## caches matrix inverses so that it does not have to recalculate
## the first function creates the matrix object
## the second function solves for the inverse and caches it
## Example:
## a <- makeCacheMatrix(matrix(c(1,2,3,4),2,2))
## # creates a 2x2 matrix
## cacheSolve(a)
## # returns the inverse of the 2x2 matrix created
## a$getinverse() 
## # returns the same inverse calculated by cacheSolve above
## a$set(matrix(c(5,6,7,8),2,2))
## # assigns a new matrix to the object in a
## cacheSolve(a)
## # checks if the inverse already exists... since it does not, calculates
## # a new inverse prints and caches it 

## function for the object which creates the matrix. the set function also
## checks if the matrix is the same as the one already stored
## returns a list of functions

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y = matrix()) {
    
    
    ## checks if it is the same matrix
    if(nrow(x) == nrow(y)) {
      
      if(sum(x == y) != length(x)) {
        ## if not, it updates the inverse and the data
        x <<- y
        inv <<- NULL  
      } else {
        print("No change in matrix!")
      }
    } else {
      ## since it is square matrix, if there are different
      ## number of rows, then it is a different matrix
      x <<- y
      inv <<- NULL
    }
  }
  get <- function() x
  getinverse <- function() inv
  setinverse <- function(inverse) inv <<- inverse
  
  list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)
}


## this function checks if the inverse of the matrix is already solved
## if it is, then it prints out the matrix
## if not, it solves for the inverse and caches it 

cacheSolve <- function(a, ...) {
  inv <- a$getinverse()
  # checks if the inverse is already calculated (cached)
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # if not, it calculates a new one
  data <- a$get()
  inv <- solve(data, ...)
  a$setinverse(inv)
  inv
}