## makeCacheMatrix accepts a matrix. It has functions to cache the matrix and its inverse
## cacheSolve returns cached inverse if available, else computes inverse, caches it and returns it

## makeCacheMatrix accepts a matrix. It has functions to cache the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
       # inverse_m is used to store the inverse of matrix. Initialized to NULL
        inverse_m <- NULL
        
        # cache the passed matrix and initialize the inverse of the matrix to NULL
        set <- function(y) {
                x <<- y
                inverse_m <<- NULL
        }
        
        # return the cached marix
        get <- function() x
        
        # cache the passed inverse
        setinverse <- function(inverse_matrix) inverse_m <<- inverse_matrix
        
        # return the cached inverse
        getinverse <- function() inverse_m
        
        #list all the functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## returns cached inverse if available, else computes inverse, caches it and returns it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_m <- x$getinverse()
        
        # if inverse found in cache return the cached inverse
	if(!is.null(inverse_m)) {
		message("getting cached data")
		return(inverse_m)
	}
	
	#if cached inverse not found, get matrix
	data <- x$get()

	#compute inverse 
	inverse_m <- solve(data)

	#cache inverse
	x$setinverse(inverse_m)

	# return inverse
        inverse_m
}
