## Writing a function that will create a cache of a matrix inversion function

## This function creates a special "matrix" object that can cache its inverse.
## This is basically a list of 4 function as defined below
##    set the value of the matrix
##    get the value of the matric
##    setinverse the cached value of the inverse of matrix
##    getinverse the previously cached value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  ##initializing the inverse matrix cache as NULL      
  cache_mat <- NULL
        set <- function(y) {
                x <<- y
                cache_mat <<- NULL
        }
  ## This calculate the inverse of matrix by calling the solve function
  ## which  inreturn stores in the setinverse function 
        get <- function() x
        setinverse <- function(solve) cache_mat <<- solve
        getinverse <- function() cache_mat
  ## the result of makeCacheMatrix function will generate a list containing
  ## the four defined functions as set , get, setinverse, getinverse
  ## and the environment where it is stored
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix function above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
cache_mat <- x$getinverse()
        if(!is.null(cache_mat)) {
                message("getting cached data")
                return(cache_mat)
        }
        data <- x$get()
        cache_mat <- solve(data, ...)
        x$setinverse(cache_mat)
        cache_mat
}
        ## Return a matrix that is the inverse of 'x'
