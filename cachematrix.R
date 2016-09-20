## The functions below use one another to shorten the length of time of the system to run
## by caching and presenting to one another matrix inverse that has already been computed.
## If the matrix inverse was never befor computed it becomes computed for the first time by 
## the cachesokve function and this information is then cached.  

## Write a short comment describing this function
## MakeCacheMatrix is a function that gives back a list of functions (with their assigned/cached values)
## It first sets a value of a matrix and gives null value to the inverse of that matrix
## Next it defines where to get the value for that matrix and later for it's inverse 
## This list of functions (and their repsective values) can be used by another function because we used
## the special <<- operator that assigns values to variables in the parent environment and thus allows
## other functions to access these variables and also define/redfine their values. 

makeCacheMatrix <- function(x = matrix()) {
        minv <- NULL
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) minv <<- inverse
        getinverse <- function() minv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function
## This function looks for cached value of the inverse of matrix x (line 36). 
## if it doesn't find such value (line 37) it will get the matrix from makecachedmatrix 
## function (line 41)
## and compute the inverse (line 42) of x and store it in the list variable called $setinverse 
## (line 46) that is defined by the makecachematrix function. 
## It can do that because the environmed that these functions/variables are defined in that are located 
## parent environment of both functions.

cacheSolve <- function(x, ...) {
        minv <- x$getinverse()
        if(!is.null(minv)) {
                message("getting cached data")
                return(minv)
        }
        data <- x$get()
        minv <- solve(data, ...)
        x$setinverse(minv)
        minv
        ## Return a matrix that is the inverse of 'x'
}
