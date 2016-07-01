## Matrix inversion is quite costly computation in terms of memory and runtime. 
## Hence caching the inverse of matrix is quite beneficial rather than comptuting it repeatedly
## Below mentioned two functions are used to cache the inverse of a matrix. 

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get teh value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<-y
                inv <<-NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<-inverse
        getInverse <- function() inv
        list(set = set,get=get,setInverse = setInverse,getInverse = getInverse)
}

## Below function calculates the mean of the special "vector" created with the above function.
## It first checks to see if the mean has already been calculated. If so, it gets the mean from
## the cache and skips the computation. Otherwise, it calculates the mean of the data and sets 
## the value of the mean in the cache via the setmean function.
## ## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
                inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
