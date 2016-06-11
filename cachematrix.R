## The following functions cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated,(and
## the matrix has not changed), then the cacheSolve should retreive
## the inverse from the cache.

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


## Testing functions

## X <- matrix(rpois(16,2), nrow = 4)
## cX <- makeCacheMatrix(X)
## cX$get()
##   [,1] [,2] [,3] [,4]
## [1,]    0    2    1    0
## [2,]    4    5    1    2
## [3,]    2    1    1    1
## [4,]    4    1    4    4

## cacheSolve(cX)
##  [,1]  [,2] [,3]  [,4]
## [1,] -0.15 -0.15  1.3 -0.25
## [2,]  0.20  0.20 -0.4  0.00
## [3,]  0.60 -0.40  0.8  0.00
## [4,] -0.50  0.50 -2.0  0.50

## cacheSolve(cX)
## getting cached data
##       [,1]  [,2] [,3]  [,4]
## [1,] -0.15 -0.15  1.3 -0.25
## [2,]  0.20  0.20 -0.4  0.00
## [3,]  0.60 -0.40  0.8  0.00
## [4,] -0.50  0.50 -2.0  0.50

     