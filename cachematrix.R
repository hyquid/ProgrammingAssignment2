## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix function creates a list of functions to
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
          x <<- y
          inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
              message("getting cached data")
              return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
