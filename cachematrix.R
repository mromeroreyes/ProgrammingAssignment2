## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
## This function will calculate the matrix inverse.
## You can set any matrix, for evaluate it (if possible set an inverse)
## 

makeCacheMatrix <- function(x = matrix()) {

	i <- NULL
	
	setValues <- function(m){
		x <<- m
		i <<- NULL
	}
	
	getValues <- function() x
	
	setInverse <- function(){
		if (!is.nan(det(x))){
			if (det(x)!=0) i <<- solve(x)
		}
		
		if (is.null(i)) {
			message("Inverse matrix can't be calculate.")
		}
	}
	
	getInverse <- function() i
	
	list(setValues = setValues, getValues = getValues, setInverse = setInverse, getInverse = getInverse)
	
}


## Write a short comment describing this function
##
## This function try to get a inverse matrix already calculated (saved in cache),
## otherwise will run calculus.
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getInverse()
		
		if (!is.null(i)){
			message("Getting cached data.")
			return(i)
		}
		
		m <- x$getValues()
		
		if (!is.na(m)) {
			
			if (det(m) != 0) {
				return solve(m)
			}
			
		}
		
		message ("Inverse matrix can't be calculate.")
		
}
