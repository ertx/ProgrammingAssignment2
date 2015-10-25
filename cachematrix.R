## makeCacheMatrix returns the four functions set, getmatrix, setinv, getinv defined below.

makeCacheMatrix <- function(amatrix = matrix()) {
  # Sets or resets amatrix and cachedinverse to NULL to be 
  # sure a new inverse is calculated when makeCacheMatrix is called.  
  # The function also sets amatrix@amatrix and amatrix@cachedinverse to NULL.  		
  
  cachedinverse<-NULL # cachedinverse is not the same variable as cachedinverse<<-NULL
  set<-function(y){
    amatrix<<-y
    cachedinverse<<-NULL
  }
  
  # getmatrix assigns amatrix   
  # setinv assigns the inverse matrix to cachedinverse, and  
  # getinv returns cachedinverse. 
  # amatrix$setinv is called only if there is no cached inverse
  
  getmatrix<-function() amatrix
  setinv<-function(inverse) cachedinverse<<- inverse
  getinv<-function() cachedinverse
  list(set=set, getmatrix=getmatrix,
       setinv=setinv,
       getinv=getinv)
}

## cacheSolve calculates the inverse of the 
## matrix passed to makeCacheMatrix, and returns its inverse.  
## If the inverse has already been calculated 
## (and the matrix passed to makeCacheMatrix has not changed), cacheSolve  
## retrieves the inverse assigned to amatrix$getinv().

cacheSolve <- function(amatrix=matrix(), ...) {
  
  # Get the cached inverse from amatrix$getinv().  
  # If amatrix$getinv()is not empty, return its value.
  # If amatrix$getinv()is empty, calculate the inverse, 
  # assign it to amatrix$setinv(), and return it.
  
  cachedinverse<-amatrix$getinv()
  if(!is.null(cachedinverse)){
    message("getting cached data")
    return(cachedinverse)
  }
  # Given that amatrix$getinv() is NULL, calculate a new inverse
  # Assign amatrix$setinv the inverse
  # Return the inverse
  
  newmatrix <- amatrix$getmatrix() 
  cacheinverse<-solve(newmatrix, ...)
  amatrix$setinv(cacheinverse)
  cacheinverse
}

