## Below two functions work together to cache inverse of any given matrix

## The below function create a special object which sets and gets the value of a invertible matrix, also sets and gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
                }
        get <- function() x
        setinverse <- function(z) inverse <<-z
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Below function calculates the inverse of the object created with the above function
## It first checks to see if the inverse has already been calculated. If so, it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached inverse")
                return(inverse)
                }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
