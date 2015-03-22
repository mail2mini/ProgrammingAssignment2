# Below are a pair of functions to cache the inverse of a matrix.

# Function to create a "special matrix" object that 
# can cache its inverse.
# It takes an invertible matrix as input.
makeCacheMatrix <- function(x = matrix()) {
  
  # variable to store the inversed matrix
  matInverse <- NULL
  
  # set function to store the matrix to be inversed. Uses <<- operator.
  set <- function(y) {
    x <<- y
    matInverse <<- NULL
  }
  
  # get function to fetch the matrix to be inversed.
  get <- function() x
  
  # set function to store the inversed matrix. Uses <<- operator.
  setInverse <- function(inverse) {
    matInverse <<- inverse
  }
  
  # get function to fetch the inversed matrix.
  getInverse <- function() matInverse
  
  # A list with named elements are returned. 
  # The elements are the 4 functions defined in the main function makeCacheMatrix().
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# Function to compute the inverse of the special "matrix" returned by the makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then this function 
# retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  # get the inversed matrix from the input object x.
  matInverse <- x$getInverse()
  
  # If the inversed matrix is not null, then return the inversed matrix from cache. 
  if(!is.null(matInverse)) {
    print("Getting matrix inverse from cached data")
    return(matInverse)
  }
  
  # If not, get the matrix to be inversed from the object x
  matData <- x$get()
  
  # use the R function solve() to inverse the matrix
  matInverse <- solve(matData, ...)
  
  # set the inversed matrix in object x
  x$setInverse(matInverse)
  
  # return the inversed matrix
  matInverse
}


## Steps to test:
## source("cachematrix.R") - source the R file

## testMatrix <- matrix(runif(25,1,100),5,5) - create a 5 x 5 invertible matrix
## specialMatrix <- makeCacheMatrix(testMatrix) - create the special matrix object
## cacheSolve(specialMatrix) - 1st execution - No cache, compute the inverse of the matrix
## cacheSolve(specialMatrix) - 2nd execution - Get the inverse from cache 