## The function makeCacheMatrix is creating a matrix inverse
# matrix inverse is a costly computation.  this function makes the 
#inversion process cached



## makeCacheMatrix function returns list of 4 functions
# set
# get
# setmean
# getmean

# input to function makeCacheMatrix is always a function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL 
        
        set <- function(y) {
                x <<- y  #pass it to global environment
                m <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(matrixX) m <<- matrixX #also pass to global environment
        
        getinverse <- function() m
        
        return(
                list(set = set, 
                     get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
        )

}


## this function returns a a matrix that is inverse of input x

cacheSolve <- function(x, ...) {
        
        m <- x$getinverse() # - this would not work if x did not exist in the environment of makeCacheMatrix()
        
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        
}
