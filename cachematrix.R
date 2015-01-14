## Below makeCacheMatrix will create a special matrix based on input matrix type argument.
## It will calculate inverse of that matrix and cache its value. When cacheSolve 
## function is called repetitively for calculating inverse without changing input matrix,
## cached value will be used instead of computing it again and again.

## makeCacheMatrix function will accept matrix type object as an argument and verify if the input
## matrix is singular or not.
## If input matrix is singular, it will display a message to modify the argument as singular matrix
## is not invertible.
## If not singular, function will compute inverse of matrix and cache its value.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
	if (det(x) == 0) {
	  message("provided matrix is singular. Singular matrix is not invertible")
	setsolve <- NULL
	} else {
	  setsolve <- function(solve) m <<- solve
	}
        getsolve <- function() m
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve) 
}


## cacheSolve function will take matrix generated from makeCacheMatrix function as input and 
## compute inverse of that matrix if its first call for that function. 
## When this function is called for computing inverse, it will verify if value is already available
## in cache for that input matrix. If available, that value is displayed else will be computed. 

cacheSolve <- function(x, ...) {
	m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
	if (det(data) == 0) {
	  message("provided matrix is singular. Singular matrix is not invertible")
	  } else {
        m <- solve(data, ...)
        x$setsolve(m)
        m 
	  }
}
