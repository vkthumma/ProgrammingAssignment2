## This file contains two functions which are helpful in caching
## the inverse of a matrix. Since matrix inversion is usually a 
## costly computation and there may be some benefit to caching the 
## inverse of a matrix rather than compute it repeatedly


## This function creates a special matrix object that can cache its inverse
## It takes an input, a regular matrix, for which an inverse needs to be 
## calculated and returns a special matrix.
makeCacheMatrix <- function(x = matrix()) {
    ##Initialize the inverse matrix value to NULL
    m <- NULL

	##Function to set the regular matrix
	##This is initializes the inverse matrix to NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }

	##This function returns the regular matrix
    get <- function() {
		x
	}

	##This function stores a value in the cache
    setinverse <- function(inverse) {
		m <<- inverse
	}

	##This function returns stored value from the cache
    getinverse <- function() {
		m
	}

	##Construct and return a list of all the above functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


##This function is a wrapper of the solve function, that takes the
##special matrix object, created using the makeCacheMatrix function
##and returns the inverse of the matrix either from a cache or
##newly constructed
cacheSolve <- function(x, ...) {
    ##Get the inverse of the matrix from a cache
    m <- x$getinverse()

	##Check if the matrix is not null. If so, return the cached version
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
	
	##Else, get the original matrix from the special matrix
    data <- x$get()
	
	##Get the inverse of the original matrix
    m <- solve(data, ...)
	
	##Store the inverse in the cache
    x$setinverse(m)
	
	##Return the inverse
    m
}
