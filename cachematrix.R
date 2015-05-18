# This pair of functions, 'makeCacheMatrix' and 'cacheSolve', cache the inverse
# of a matrix. This minimizes the need for repeated costly computation of matrix
# inversion.

# The 'makeCacheMatrix' function creates a special "matrix" object that can 
# cache its inverse. It creates a list containing a function to
# 
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse
# 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        # initiate value of inverse
        i <- NULL
        
        # set value of matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        # get value of matrix
        get <- function() x
        
        # set value of inverse
        setinverse <- function(inverse) i <<- inverse
        
        # get value of inverse
        getinverse <- function() i
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# The 'cacheSolve' function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.
# Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
        # check to see if inverse has already been calculated
        i <- x$getinverse()
        
        # if so, get the inverse from the cache and skip the computation
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        # otherwise, calculate the inverse from the data
        data <- x$get()
        i <- solve(data, ...)
        
        # set the value of the inverse in the cache via the 
        # 'setinverse' function
        x$setinverse(i)
        
        # return the value of inverse
        i
}
