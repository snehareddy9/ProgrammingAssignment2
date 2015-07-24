## makeCacheMatrix and cacheSolve functions caches the inverse of a matrix so that
## when we want to find the inverse with the same values we can retrieve it from 
## a cache instead of computing it all over again

#this function helps in caching the inverse
makeCacheMatrix <- function(x=matrix())
{
  
  i <- NULL
  # Setting the value of matrix
  set <- function(y)
  {
    x <<- y
    i <<- NULL
  }
  #get the value of matrix
  get <- function() x 
  #sets the value of inverse
  setInverse<-function(inverse) i<<-inverse
  #gets the value of inverse
  getInverse<-function() i
  #list containing all the functions
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}
##this function retrieve the matrix inverse if it is the cache
##calculates the inverse if its not in the cache
cacheSolve <- function(x) {
  
  #assigns inverse of matrix if it is present in the cache
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  # if the matrix inverse is not in cache it calculates it using the solve function
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  i
  #returns the matrix inverse
}
