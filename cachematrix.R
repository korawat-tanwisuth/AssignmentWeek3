## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This function returns getter and setter for our special matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <-  NULL
  set <- function(y){
    # We use double assignment operator to assign to variable in the parent environment.
    # If we set a new value for our matrix, we have to reset the inverse to null so that we recompute the value of it later.
    x <<- y
    inv <<- NULL
  }
  get <- function(){
    return(x)
  }
  setinv = function(inverse){
    inv <<- inverse
  }
  getinv <- function(){
    return(inv)
  }
  #This function returns a list of getter and setter that can be called by $ operator.
  return(list(set=set, get=get, setinv=setinv, getinv=getinv))
}


## Write a short comment describing this function

#This function takes in the previous function as an input and determine whehter the inverse has been calculated.
#If there exists an inverse, the function will simply get it using the getter method. 
#Otherwise, it will recompute the inverse of the matrix and restore the cache for future use.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x''
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  return(inv)
}
