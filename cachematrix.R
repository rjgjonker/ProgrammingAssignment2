# cachematrix.R
# 
# My solution to Programming Assignment 2: Lexical Scoping in the Coursera
# course 'R Programming' by Roger D. Peng.
#
# This file provides two functions: one that creates a matrix object that
# contains a cache to store its inverse counterpart and another that returns
# the inverse of a matrix created using this function, using the cached value
# when available and filling it otherwise.
# The implementation of the functions inspired by the example for a vector
# with a cached mean given in the exercise.

# The function makeCacheMatrix returns a list containing a cache variable for 
# an inverted matrix and accessor functions for the matrix and its inverse
# counterpart, respectively. set() and get() set and get the matrix itself,
# while setinverse() and getinverse() get and set the cached inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	
	# Setter and getter function for the matrix
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	
	# Setter and getter function for the inverse value
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	
	# Return a list containing the above functions
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}

# The function cacheSolve(x) returns the inverse of the matrix contained in the
# variable x, which asssumed to be invertible. If x already has a cached inverse
# matrix, this is returned. Otherwise, it is computed using the built-in  R
# function solve(), which in turn employs LAPACK linear algebra functions for
# solving the matrix numerically. Once computed, the inverse matrix is stored in
# the cache through the accessor function for x.

cacheSolve <- function(x, ...) {
	# First, check whether x already has a cached inverse and return it if it does
	inv <- x$getinverse()
	if(!is.null(inv)) {
		return(inv)
	}
	
	# If not, we need to fetch the contents and invert the matrix using solve()
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}
