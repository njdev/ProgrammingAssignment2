## Put comments here that give an overall description of what your
## functions do

# When "makeVector" is called (w/an invertible matrix argument) and
# "cacheSolve" is then called w/the result as an argument, the 
# inverse matrix results. On the first call to "cacheSolve",
# the inverse is solved; however, on subsequent calls
# the cached value is used ("cache" here means the "invcache"
# var in the outer function environment.)

## Write a short comment describing this function

# Takes a matrix argument, and results in a vector of
# four function references. These functions are called
# in "cacheSolve" to get the data ("get"), set and get the
# cached value in this function's local variable "invcache"
# ("setinv" and "getinv".)
makeVector <- function(x = matrix()) {

        invcache <- NULL
        set <- function(y) {
                x <<- y
                invcache <<- NULL
        }
        get <- function() x
        setinv <- function(inv) invcache <<- inv
        getinv <- function() invcache
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Write a short comment describing this function

# Note: this only works for matrices that have an inverse.
# Gets the inverse in cache if it's there. If not 
# ("invcache" is NULL) calls solve() to calc the inverse, 
# then sets the inverse in cache for future calls. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invcache <- x$getinv()
        if(!is.null(invcache)) {
                message("getting cached data")
                return(invcache)
        }
        data <- x$get()
        invcache <- solve(data, ...)
        x$setinv(invcache)
        invcache
}
