## Function makeCacheMatrix creates a special "matrix", that contains
## the following list of functions:
##
## set : set the value of the matrix
## get : get the value of the matrix
## setsolve: set the inverse matrix
## getsolve: get the inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
   x<-NULL
   set <- function(y) {
      x <<-y
      m <<-NULL
   }
   get <- function() x
   setsolve <- function(solve) m <<-solve
   getsolve <- function() m
   list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## The function cacheSolve calculates the inverse of the special "matrix"
## created with the makeCacheMatrix function. It first checks to see if the
## inverse matrix has already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, it calculates the new
## inverse of the matrix and sets the values of the inverse in the cache via
## setsolve function.
##
## Some examples of the execution can be:
#PROCESS:
#>source(
#>my_mat <-makeCacheMatrix()
#>my_mat$set(matrix(c(1, -0.35, -0.35, 1), nrow=2, ncol=2))
#>my_mat$get()
#>cacheSolve(my_mat)
#>cacheSolve(my_mat)
#>my_mat$set(matrix(c(1, -0.35, -0.35, 1,1,0.35,-0,35,1), nrow=3, ncol=3))
#>my_mat$get()
#>cacheSolve(my_mat)
#>cacheSolve(my_mat)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
