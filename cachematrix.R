## This word has been done by Hamoud Alshammari to calculate the
## Invertion of a given matrix, so there are two functions
## First function is to setting and getting the matrix and the inverted matrix
## Second function is to calculating the inverse matrix if it has not been calculated

## makeCacheMatrix function has four sub functions:
## 1- set: to set the matrix values.
## 2- get: to return the values in the matrix.
## 3- setInverse: to set values to the inverted matrix.
## 4- getInverse: to get the values of the inverted matrix.
## So, we need to put them all in a list.

makeCacheMatrix <- function(x = matrix()) 
{
  inverse_x<- matrix(NA)
  
  set<- function(y)
  {
    x<<- y
    inverse_x<<- matrix(NA)
  }
  
  get<- function()
  {
    x
  }
  
  setInverse<- function(y)
  {
    inverse_x<<- y
  }
    
  getInverse<- function()
  {
    inverse_x
  }
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cashSolve function is a function that calculates the invertion of a givin matrix
## first it tests whether the inversion has been calculated or not.
## if it has been calculated, the function will NOT calculate it again but re-read it from cache.
## Otherwise, calculates it and returns the values.

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  inverse_x<<- x$getInverse()
  
  if (!is.na(inverse_x))
  {
    message("getting cached data")
    return(inverse_x)
  }
  data<- x$get()
  inverse_x <- solve(data)
  x$setInverse(inverse_x)
  x$getInverse() 
}
