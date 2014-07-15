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


## cacheSolve function expects a makeCacheMatrix instance as its argument.
## cacheSolve function first check whether the inverse matrix has been solved
## and stored in the makeCacheMatrix instance.
## If yes, it will directly retrieve the cached inverse matrix. 
## If the inverse matrix is not solved (a.k.a. null),
## this function will get the matrix as set using makecacheMatrix function from
## the makeCacheMatrix instance and solve the inverse matrix, store it
## back in the makeCachMatrix instance and print it out on the console.

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
