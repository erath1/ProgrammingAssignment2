## Taking advantage of the lexical scoping rules, we create two different functions i.e.
##makeCacheMatrix function would create a special "matrix" object that can cache its inverse whereas
##the cacheSolve function would compute the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
##Assumption: Matrix supplied is alwas invertible.

## x: a square invertible matrix
## return: a list containing functions to set the matrix, get the matrix
##                                        set the inverse and get the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    # `<<-  is used` to assign a value to an object in an environment 
    # different from the current environment
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
  ## x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
}
#test function  takes in any invertible matrix
##calculates its inverse twice using the above functions
##prints out the times it takes for both runs. 
test = function(mat){
  ## mat: an invertible matrix
  
  temp = makeCacheMatrix(mat)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
}

set.seed(1110201)
r = rnorm(2000000)
mat1 = matrix(r, nrow=1000, ncol=1000)
test(mat1)
##ime difference of 3.629402 secs
# getting cached data
## Time difference of 0.295197 secs