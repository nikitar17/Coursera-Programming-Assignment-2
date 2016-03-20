makeCacheMatrix <- function(x = matrix()) {

inv <- NULL
        set <- function(y) { ## set the value of the matrix
                x <<- y
                inv <<- NULL
        }
        get <- function() x  ## get the value of the matrix
        setInverse <- function(solve) inv <<- solve  ## set the inverse of the matrix
        getInverse <- function() inv   ## get the inverse of the matrix
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)


}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
