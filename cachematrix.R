## The objective is to write function to achieve efficient Matrix Inverse
## First checking if the inverse already exists and inverse it if does not exist in cache already
## These functions are modeled on the same lines as the mean example given

##The first function, `makeCacheMatrix` creates a special "vector", which is
##really a list containing a function to

##1.  set the value of the Matrix
##2.  get the value of the Matrix
##3.  set the value of the inverse
##4.  get the value of the inverse

## makeCacheMatrix creates the matrix
## first initilalized invers matrix wit NULL values, then creates cache


makeCacheMatrix <- function(x = matrix()) {
  invers <- NULL
  set <- function(y) {
    x <<- y
    invers <<- NULL
  }
  get <- function() x
  setinver <- function(inv1) invers <<- inv1
  getinvers <- function() invers
  list(set = set, get = get,
       setinvers = setinvers,
       getinvers = getinvers)
}


## casheSolve first checks if the inverse of x exists in cache.
## if exists cache, it retrieves the inverse and returns
## if does not exist, it calculates the inverse, caches it and returns the inverse
## main assumption is that input x matrix is always square and invertible
## ginv() from MASS package can be used in place of solve() in the input matrix is not square matrix
## as per the instructions, input matrix x is assumed to be square always


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## call the matrix inverse from cache
  invers <- x$getinvers()
  ## check if the inverse matrix is not null and return inverse if not null
  if(!is.null(invers)) {
    message("getting cached maxtrix x's inverse")
    return(invers)
  }
  data <- x$get()
  ## use Solve function to get the inverse of the sqaure matrix data
  invers <- solve(data)
  ## cache the matrix's inverse
  x$setinvers(invers)
  ## return the maxtrix's inverse as matrix
  return(invers)
}
