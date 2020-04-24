## The first function, makeCacheMatrix creates a special "matrix" that
## 1. set the values of the matrix
## 2. get the values of the matrix
## 3. set the value of its inverse
## 4. get the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
    Inv <- NULL
    set <- function(y) {
        x <<- y
        Inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) Inv <<- inverse
    getinverse <- function() Inv
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve Function will look into cache to see if 
## the inverse exist for matrix and return the value. 
## The outcome will be "getting cached data" if the inverse 
## is already cached in memory 

cacheSolve <- function(x, ...) {
    Inv <- x$getinverse()
    if (!is.null(Inv)) {
        message("getting cached data")
        return(Inv)
    }
    data <- x$get()
    Inv <- solve(data, ...)
    x$setinverse(Inv)
    Inv
}

# test
myMatrix <- matrix(c(1:4), 2, 2)
myCache <- makeCacheMatrix(myMatrix)
cacheSolve(myCache)

# re-test
cacheSolve(myCache)