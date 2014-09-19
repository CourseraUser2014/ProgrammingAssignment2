## Put comments here that give an overall description of what your
## functions do
## edited by Courserauser2014 on 19/9/2014

## The function 'MakeCacheMatrix' initializes a variable with the given 
## numbers of a (invertable) matrix. Additionaly, a placeholder for storing 
## the inverse of the matrix is initialized (with Null).
## The (values of) the matrix can be witten resp. read with the set resp get 
## functions. 
## The (values of) the inverse of the matrix can be witten resp. read with 
## the setinverse resp getinverse functions. 


makeCacheMatrix <- function(x = matrix()) {
	  #set variable inverse to NULL	
        inverse <- NULL

	  #set function set x to the argument y and set inverse to NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }

	  #return the matrix x (argument of makeCacheMatrix)
        get <- function() x

	  #set the inverse matrix 
        setinverse <- function(inverse_mat) inverse <<- inverse_mat	
	  	
	  #return the inverse matrix
        getinverse <- function() inverse

	  #returns a labeled vector of functions set, get, setinverse, getinverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function cacheSolve retrunes the cached inverse matrix of the
## argument/variable if the inverse matrix has already been calculated
## (not null anymore) or calculates and sets the inverse matrix of
## the given argument/variable to the result of the Solve-function
## (inverting a matrix given a invertable matrix)

cacheSolve <- function(x, ...) {

        # Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()

	  #if not null, a inverse matrix was cached, so return inverse
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }

	  #since inverse is null (not yet calculated, set data to x (from makeCacheMatrix)
        data <- x$get()

	  # set inverse to the result of the Solve-funtion (inverting)
        inverse <- solve(data,...)

	  #return the inverse matrix
        x$setinverse(inverse)
}