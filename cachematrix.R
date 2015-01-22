## makeCacheMatrix - Creates the inverse of a matrix
## cacheSolve - Returns the inverse of a matrix from cache if already
## computed else it adds the value to the cache

## makeVector` creates a special "matrix", which is really a list 
##containing a function to set and get the value of matrix, set and get the 
## value of matrix inverse

makeCacheMatrix <- function(x = matrix()) 
{
  ##Initializes an empty matrix
  m<-NULL
  
  ##Sets the matrix value
  set<-function(y)
    {
      x<<-y
      m<<-NULL  
    }
  
  ##Gets the value of the matrix
  get<-function() x
  
  ##Sets the value of the matrix inverse
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  
  ##Sets the value of the matrix inverse
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
  
}


## This function computes the inverse of the special
##"matrix" returned by `makeCacheMatrix` above. If the inverse has
##already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  m<-x$getmatrix()
  
  ##If inverse of matrix exists in cache, retrive from the cache
  if(!is.null(m))
    {
    message("getting cached data")
    return(m)
    }
  
  ##Else compute the inverse of matrix
  matrix<-x$get
  m<-solve(matrix, ...)
  
  ##Set the value of the inverse matrix
  x$setmatrix(m)
  m  
}
