## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function()(x)
    setinv <- function(inverse)(inv <- inverse)
    getinv <- function()(inv)
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  }


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
        ## Return a matrix that is the inverse of 'x'

## Testing Above Functions
> m <- matrix(rnorm(16),4,4)
> m1 <- makeCacheMatrix(m)
> cacheSolve(m1)
[,1]        [,2]        [,3]        [,4]
[1,]  0.2312713 -0.35753060  0.06456288  0.11107567
[2,]  0.1971115 -0.21343520  0.50983496 -0.46762888
[3,]  0.3399946  0.08662556 -0.83603688  0.03187723
[4,] -0.6577003 -0.22566227  0.27541660 -0.04874529
