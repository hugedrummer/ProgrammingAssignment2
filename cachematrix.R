makeCacheMatrix <- function(x = matrix()) {
  #I initialized the mean to NULL during the first call to makeVector
  #then I  first calculate the mean in cachemean, 
  #then I named i for the inverse 
  i <- NULL 
  set <- function(y) {
    #I used the <<- operator to set the value of x and i
    # to modify x and i defined in the enclosing environment
    x <<- y
    i <<- NULL
    #then I reset i to NULL to modify the underlying
    #vector
  }
  get <- function() x
  #I set the mean of the vector x which is Called by cachemean.
  setinverse <- function(inverse) m <<- inverse
  
  getinverse <- function() i
  #The return value of the makeVector function is a list
  #of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  #this function will return the inverse of the variable i
  i <- x$getinverse()
  if(!is.null(i)) {
    #the if loop will read the message if i is not NULL
    message("getting cached data")
    return(i)
    #the return statment will complete the function
  }
  data <- x$get()
  i <- solve(data, ...)
  #finally, this will solve the function from data
  x$setinverse(i)
  i
}

