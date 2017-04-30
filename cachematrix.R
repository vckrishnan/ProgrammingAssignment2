## This function demos the operation computing and returing the value from cache.

## Function makeCacheMatrix performs four operations
## 1. set the Matrix
## 2. get the Matrix
## 3. set the Inverse of Matrix
## 4. get the Inverse of Matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set<-function(y){
    x <<- y
    m<<-NULL
  }
  get <- function() x
  setInverse <- function(mInverse){
    if(is.matrix(mInverse) == T){
      m <<- solve(mInverse)
    }else{
      print ("Function Expects Matrix as its argument")
    }  
  }
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Function cacheSolve checks is the Inverse matrix available in the cache or not, if it is available then the inverse matrix is printed from the cache
## if not then Inverse of matrix is calculated

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matdata <- x$get()
  m <- solve(matdata)
  x$setInverse(m)
  m
}
