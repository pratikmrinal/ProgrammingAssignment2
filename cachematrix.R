## This function is used to take the matrix argument and creates a list to set and get value of the matrix. In addition, it 
## also sets and gets the value of its inverse.

makeCacheMatrix <- function(x = matrix()) {
    x1 <- NULL
    set <- function(y) {
        x <<- y
        x1 <<- NULL
    }
    get <- function() x
    setinv <- function(inv) x1 <<- inv
    getinv <- function() x1
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## The function takes the output of the previous function as its input and check for the inverse value present in cache.
## If not found, it obtains the data using get function and calculates the inverse of matrix using solve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x1 <- x$getinv()
        if(!is.null(x1)) {
                message("getting cached data")
                return(x1)
        }
        data <- x$get()
        x1 <- solve(data)
        x$setinv(x1)
        x1
}
