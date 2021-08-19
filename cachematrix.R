# Since running the same computational function repeatedly, especially if you 
# have a large vector of data, can be potentially time-consuming, it may be 
# worth cache-ing (saving to the disk) the values returned from a function so 
# that these can later on be retrieved from the cached object rather than having
# to compute the function again.

# The following functions can be used to cache the inverse of a matrix and later
# on compute the inverse of the same matrix


# The first function, makeCacheMatrix, creates a vector which caches the inverse
# of a matrix specified by the formal argument x

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# The following function, cacheSolve, calculates the matrix inverse of the vector
# created by the makeCacheMatrix function above, x. If the matrix inverse has 
# already been calculated, it will return the cached value without having to 
# calculate its value all over again. Otherwise, it will calculate the matrix 
# inverse of the data and set the value of the matrix inverse in the cache 
# (disk) via the setinverse() function

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
