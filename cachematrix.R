## The folllowing functions allow to cache the inverse of a matrix rather than compute it repeatedly 

##This function creates a special "matrix" object that can cache its inverse. 
makeCacheMatrix <- function(x = matrix()) 
{
  s <- NULL       
  set <- function(y) ##Set the value of the matrix
    {
      x <<- y
      s <<- NULL
    }
  get <- function() x  ##Get the value of the matrix
  setInverse <- function(solve) s <<- solve   ##Set the inverse matrix
  getInverse <- function() s                  ##Get the inverse matrix
  list(set = set, get = get,setInverse = setInverse,getInverse = getInverse) #return a list containing the 4 previous functions

}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
##above. If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        s <- x$getInverse()            ##Query the x matrix's cache 
  if(!is.null(s))                      ##If there is a cache
    {   
      message("getting cached data")
      return(s)                        ##Return the cache, without computation needed
    }
  data <- x$get()                      ##If there's no cache
  s <- solve(data, ...)                ##Compute the inverse matrix
  x$setInverse(s)                      ##Save the result back to x's cache
  s                                    ##Return the result: inverse matrix

}
