## The following two functions, makeCacheMatrix & cacheSolve, 
## compute the inverse of a matrix and cache the result so that
## the result can be looked up instead of being re-computed.
## Using the cached result will speed up the process when the
## result is needed multiple times, as in a loop. 


## makeCacheMatrix() creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## function assumes that the matrix is invertible
        ## return value is a list of functions that
                ## set the matrix
                ## get the matrix
                ## set the inverse
                ## get the inverse
        ## this list becomes the input to cacheSolve()
        inv = NULL
        set = function(y) {
                # '<<-' is used to assign a value to an object in 
                # an environment external to the current environment
        x <<- y
        inv <<- NULL
        }
        get = function() x
        setinverse = function(inverse) inv <<- inverse
        getinverse = function() inv
        list(set=set, get=get, setinv=setin, getinv=getinv)
}


## cacheSolve() computes the inverse of the matrix returnd by makeCacheMatrix().
## If the matrix has not changed and the inverse has already been calculated, it
## will retrieve the inverse from cache instead of re-calculating. 

cacheSolve <- function(x, ...) {
        ## Input to this function is the output of makeCacheMatrix
        ## return value is the inverse of the matrix submitted as input
        ## to makecacheMatrix()
        inv = x$getinverse()
        
        # if the inverse has already been calculated
        if (!is.null(inv)){
                # get it from the cache and skip the computation
                message ("getting cached data")
                return(inv)
        }        
        #othersise, calculate the inverse
        matrix.data = x$getinverse()
        inv = solve(matrix.data, ...)
        
        #set the value of the inverse in the cache via the setinv function.
        x$setinverse(inv)
        return(inv)
}
