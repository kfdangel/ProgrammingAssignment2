## The below functions produce the inverse of a matrix by
## utilizing cache to save memory space and speed up computations

## Creating a function that creates a matrix that can produce an
## inverse matrix utilizing cache

makeCacheMatrix <- function( x = matrix() ) {
  
  inver <- NULL
  
  set <- function(y) {
    
    #assigning X to a different environment than the current
    x <<- y
    inv <<- NULL
    
  }
  #getting the matrix
  get <- function() x
  
  setinverse <- function(inverse) inver <<- inverse
  getinverse <- function() inver
  
  #returns a list of the methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Return the inverse of the "x" matrix

cacheSolve <- function(x, ...) {
  inver = x$getInver()
  
  # if the inverse has already been calculated, below will be executed
  if (!is.null(inver)){
    
    message("obtaining previously cached data")
    return(inver)
    
  }
  
  # If not previously calculated, the inverse is calculated 
  matrix_data = x$get()
  inver = solve(matrix_data, ...)
  
  # sets the value of the inverse in the cache
  x$setInver(inver)
  
  ##display the inverse matrix of x
  inver
       
}
