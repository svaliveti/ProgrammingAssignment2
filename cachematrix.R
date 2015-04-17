## Put comments here that give an overall description of what your
## functions do

## Pair of functions to facilitate cached usage of matrix inverse computation.
## makeCacheMatrix provides a object wrapper around matrix (to hold the matrix & inverse); cacheSolve provides the cached solve interface

## Write a short comment describing this function
## makeCacheMatrix:
##  Wrapper around matrix() providing an object (closure) to hold the matrix and its inverse
## 	Get/Set accessor functions provide read/write access (respectively)

makeCacheMatrix <- function(x = matrix()) {
	ix <- NULL

	get <- function() x
	getinv <- function() ix
	
	set <- function(y) { 
		x <<- y
		ix <<- NULL
		y	
	}	
	setinv <- function(inv) ix <<- inv
	
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## cacheSolve:
## 	Cached version of the std "solve": invoke solve only if there is no inverse stored (cached) in the object and then store the inverse

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	
	ix <- x$getinv()
	if(!is.null(ix)) { 
		message("getting cached data")
		return(ix)
	}
	
	ix <- solve(x$get(), ...)
	x$setinv(ix)
	ix
}
