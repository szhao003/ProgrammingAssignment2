#Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than
#compute it repeatedly. The goal of the following two functions is to compute the inverse of matrices and cache the inverse of 
#a matrix if the inverse has been computed.


#Step 1: This function cretes a special"matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  #set the values of the matrix
  m <- NULL
  set <- function(y) {
         x <<- y
         m <<- NULL
  }
  
  #get the values of the matrix
  get <- function() {
    x
  }

  #set the values of the inverse matrix
  setmatrix <- function(solve) {
    m <<- solve
  }
  
  #get the values of the inverse matrix
  getmatrix <- function() {
    m
  }

  list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix) 
}

#Step 2: The function below computes the inverse of the special "matrix" returned by makeCasheMatrix above. If the inverse
#has already been computed, then this function will retrieve the inverse from the cache.

cacheSolve <- function(x,...) {
   #Checks to see if the inverse matrix has already been calculated. If yes, retrive the inverse matrix from the cache.
   m <- x$getmatrix()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
  }
  
  #Computes the inverse matrix and return the inverse matrix
  matrix <- x$getmatrix()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}

