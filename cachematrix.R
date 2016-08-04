## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        i <- NULL	
        	##cache store for the inverse same as in example
        set <- function(y) {
        	##this function is for setting a new matrix in the object
                x <<- y ## set argument y as new matrix x (override the old one)
                i <<- NULL ##reset the inverse because if there is an old inverse cached, it will no longer be relevant
        }
        get <- function() x ##returns matrix, same as vector as in the example
        setinverse <- function(inverse) i <<- inverse ##sets the inverse, same as mean in the example
        getinverse <- function() i  ##returns inverse, same as mean in the example
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
        ##return a list of the data of the matrix object
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        i <- x$getinverse() ##temporarily get the cached inverse from the matrix (might be null)
        if(!is.null(i)) {
        	##Check if what we got is null, if it is not:
                message("getting cached data")
                ##then we can just return the cached value
                return(i)
        }
        ## else if it is null:
        data <- i$get() ##temporarily get the matrix data from the matrix object
        i <- solve(data, ...) ##use the solve function to calculate the matrix inverse, and feed it the matrix data
        x$setinverse(i) ##use the set function to set the calculated inverse into the matrix object
        i ##return the inverse
}
