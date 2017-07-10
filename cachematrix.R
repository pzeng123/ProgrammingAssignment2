## Matrix inversion is usually a costly computation. Here is the method to caching the inverse of a matrix.
## This file computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

## Function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse. 
## It is a list containing a function to:
## 1. set the value of a matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix
##
## Input of function "makeCacheMatrix" should be a matrix.
## For example, y <- makeCacheMatrix(matrix(c(0,1,2,1,1,-1,2,4,0),3,3))
## Output of function cacheSolve will be:
## > cacheSolve(y)
##      [,1] [,2] [,3]
## [1,]  2.0   -1  1.0
## [2,]  4.0   -2  1.0
## [3,] -1.5    1 -0.5
## > cacheSolve(y)
## getting cached data
##      [,1] [,2] [,3]
## [1,]  2.0   -1  1.0
## [2,]  4.0   -2  1.0
## [3,] -1.5    1 -0.5


makeCacheMatrix <- function(x) {
		inv <- NULL
		## 1. set the value of a matrix
		set <- function(input) {
			  x <<- input
			  inv <<- NULL
		}
		## 2. get the value of the matrix
		get <- function() x
		## 3. set the value of the inverse of the matrix
		setinverse <- function(inverse) inv <<- inverse
		## 4. get the value of the inverse of the matrix
		getinverse <- function() inv
		## create a list of the four functions, the list is the return value
		list(set = set, get = get,
			 setinverse = setinverse,
			 getinverse = getinverse)
}


## Function "cacheSolve" computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
		inv <- x$getinverse()
		## If get inverse is not null, it's cached. return cached data.
		if(!is.null(inv)) {
			  message("getting cached data")
			  return(inv)
		}
		## get inverse is null.
		## Compute inverse by solve() and cache it.
		data <- x$get()
		inv <- solve(data, ...)
		x$setinverse(inv)
		inv
}
