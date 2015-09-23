## makeCacheMatrix takes a matrix as input and returns a list of 4 functions
## which can operate on on the matrix. cacheSolve accepts the list of functions
## returned by makeCacheMatrix and returns the inverse of the matrix.  If
## this is the first time the inverse has been calculated it is cached for future
## use

## takes a matrix and returns a list of functions which operate on the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
          x <<- y
          i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## returns the inverse of a matrix, either returning from cache or solving
## the matrix and returning the solution

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
