# makeCacheMatrix: This function creates a special "matrix" object
# that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. If the inverse has already been 
# calculated (and the matrix has not changed), then 
# cacheSolve should retrieve the inverse from the cache.



#Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

	# stores the cached matrix 
	m <- NULL

	#stores the matirx 
	set <- function(y) {
		x <<- y
		m <<- NULL
	} 

	get <- function() x 

    # setMatrix sets the value of a matrix and caches the give argument 
	setmatrix<-function(solve) m <<- solve 

	# gets the cache value (the matrix)
	getmatrix<-function() m

	list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}




## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
	
	# gets the cache matrix
    m <- x$getmatrix()

    # if a cached value (maxtrix) exist return it 
    if(!is.null(m)){
    	message("getting cached data")
    	return(m)
    }
    # else get the matrix and calculate the inverse and cache it
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)

    # returns the inverse of the matrix 
    m
}