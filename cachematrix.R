## Basically what we do is exactly like given example
# but with wont work  when the matrix supplied is always invertible we can do ##

## This function creates a special "matrix" object that can cache its inverse as required

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) inv <<- solve
	getsolve <- function() inv
	list(set = set, get = get,
	     setsolve = setsolve,
	     getsolve = getsolve)

}


## This function computes the inverse of the my matrix returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
	
	inv <- x$getsolve()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	mat <- x$get()
	inv <- solve(mat, ...)
	x$setsolve(inv)
	inv
}
