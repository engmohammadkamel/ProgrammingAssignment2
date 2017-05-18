## this code shows how to cach the inverse of a matrix in order not recalculate it every time

## makeCacheMatrix creates a special "Matrix", which is really a list containing a function to
## set the value of the Matrix
## get the value of the Matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	MInverse <- null
	set <- function(y){
		x <<- y
		MInverse <<- null
	}
	get <- function() x
	setinverse <- function(Inverse){
		MInverse <<- Inverse
	}
	getinverse <- function() MInverse
	list(set =set, get=get, setinverse = setinverse, getinverse=getinverse)
}


## calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and 
## sets the value of the inverse in the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	MInverse <- x$getinverse()
	if(!is.null(MInverse)){
		message("getting cached data")
		return(MInverse)
	}
	data <- x$get()
	MInverse <- solve(data,...)
	x$setinverse(MInverse)
	MInverse
}
