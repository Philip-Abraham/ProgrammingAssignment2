### Matrix inversion is usually a costly computation and there may be some benefit to caching 
### the inverse of a matrix rather than compute it repeatedly. 

## There are two main functions that are used to create a special object that stores  a "matrix" 
## object, solves it's inverse and cache's its inverse.

## The first function, makeCacheMatrix creates the special "matrix" object, which is really a list containing a function to
##1.	set the value of the matrix
##2.	get the value of the matrix
##3.	set the value of the matrix
##4.	get the value of the matrix

## The second function, cacheSolve computes the inverse of the matrix. 
## Input of cacheSolve is the object where makeCacheMatrix is stored.
## If the inverse has already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache created from the first function.

####################################################################################################
#FIRST FUNCTION
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        mat <- NULL
        set <- function(y) { #set is a function that changes the matrix stored in the main function
                x <<- y #"x <<- y" substitutes the matrix x with y (the input) in the main function (makeCacheMatrix). 
                mat <<- NULL
        }
        get <- function() x #get is a function that returns the matrix x stored in the main function. Doesn't require any input.
        
        ## setinverse and getinverse are functions very similar to set and get. 
        ## They don't calculate the inverse, they simply store the value of the input in a variable mat into 
        ## the main function makeCacheMatrix (setinverse) and return it (getinverse).         
        setinverse <- function(inverse) mat <<- inverse
        getinverse <- function() mat
        
        ## To store the 4 functions in the function makeCacheMatrix, we need the function list(), so that when we 
        ## assign makeCacheMatrix to an object, the object has all the 4 functions.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#SECOND FUNCTION
## The following function computes the inverse of the special "matrix" returned by makeCacheMatrix function above. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates the inverse of the data and 
## sets the value of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat <- x$getinverse()
        
        ## The first thing cacheSolve does is to verify the value mat, stored previously with getinverse, exists and 
        ## is not NULL. If it exists in memory, it simply returns a message and the value mat, that is supposed 
        ## to be the inverse, but not necessarily.        
        if(!is.null(mat)) {
                message("getting cached data")
                return(mat)
        }
        
        # If it was the case, "return(mat)" would have ended the function. So everything that follows 
        # this if() is a sort of else {}. data gets the matrix stored with makeCacheMatrix, mat calculates the inverse of
        # the matrix and x$setinverse(mat) stores it in the object generated assigned with makeCacheMatrix.         
        data <- x$get()
        mat <- solve(data, ...)
        x$setinverse(mat)
        mat
}
