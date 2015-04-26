## Put comments here that give an overall description of what your
## functions do

## Create and return a list object with function elements that provide computation and caching of 
## the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL		## Initialize the cached inverse to NULL

        set <- function(y) {		## Function element used to save a matrix value and clear any previously cached inverse
                x <<- y
                inverseMatrix <<- NULL
        }

        get <- function() x		## Function element used to retrieve the current input matrix

        setInverse <- function(inverseMatrix) inverseMatrix <<- inverseMatrix
					## Function element used to save (cache) a computed inverse of the input matrix

        getInverse <- function() inverseMatrix
					## Function element used to retrieve the currently cached inverse

        list(set = set, get = get,	## Create a list of the four cache/retrieve functions and return
             setInverse = setInverse,
             getInverse = getInverse)
}


## Compute the inverse of a matrix and cache and return the result, or return a previously cached
## result if one exists

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <- x$getInverse()	## Retrieve the currently cached inverse matrix

        if(!is.null(inverseMatrix)) {	## If there is a cached value, return it
                message("getting cached data")
                return(inverseMatrix)
        }
	
        matrix <- x$get()		## Otherwise, get the current input matrix
        inverseMatrix <- solve(matrix)	## Compute the inverse
        x$setInverse(inverseMatrix)	## Cache the result
        inverseMatrix			## Return the result
}
