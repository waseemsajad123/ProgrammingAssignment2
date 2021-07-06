# R programming Coursera peer reviewed assignment,week 03


# creating a special matrix object that can cache its inverse 

makeCacheMatrix <- function(x = matrix() ) {
      
      # first we have to initialize the inverse property
      j <- NULL
      
      # setting the matrix
      set <- function(matrix){
            m <<- matrix
            j <<- NULL
      }
      
      # getting the matrix
      get <- function(){
            m
      }
      
      # setting the inverse of matrix
      setInverse <- function(inverse){
            i <<- inverse
      }
      # getting the inverse of matrix
      getInverse <- function(){
            i
      }
      
      # list of methods
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
      
}

cacheSolve <- function(x, ...) {
      
      # This will return a matrix that is the inverse of 'x'
      m <- x$getInverse()
      
      # Just return the inverse if its already set
      if( !is.null(m) ) {
            message("getting cached data")
            return(m)
      }
      
      # Getting the matrix from our object
      data <- x$get()
      
      # Calculate the inverse using matrix multiplication
      m <- solve(data) %*% data
      
      # Setting the inverse to the object
      x$setInverse(m)
      
      # Returning the matrix
      m
      
}
