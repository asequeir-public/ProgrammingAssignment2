## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix creates a special "matrix", which is a list containing functions to
#set the value of the matrix
#get the value of the matrix
#setinverse the value of the matrix inverse
#getinverse the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inv <<- inv
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#cacheSolve function calculates the inverse of the special "matrix" created with the above
#function makeCacheMatrix. 
#However, it first checks to see if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the data and sets the value of the inverse in the 
#cache via the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

testmycode <- function() {
  set.seed(1)
  mreal <- matrix (runif(100), c(10,10))
  m <- makeCacheMatrix(mreal)
  minv <- cacheSolve(m)
  str(minv)
  str(cacheSolve(m)) #2nd call, should show 'getting cached data' message on console
  
}
