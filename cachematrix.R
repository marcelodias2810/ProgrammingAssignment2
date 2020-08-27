## Put comments here that give an overall description of what your
## functions do

##The function makeCacheMatrix consists of the functions set, get, setInverse
#and getInverse, which sets the matrix, get the matrix, set the inverse of the
#matrix and get the inverse of the matrix, respectively.

makeCacheMatrix <- function(x = matrix()){
    inv <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL 
    }
    get <- function(){x}      #function to get the matrix x
    setInverse <- function(inverse) {inv <<- inverse}
    getInverse <- function() {inv}
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

#########################################

## Write a short comment describing this function
#This function is used to get the cache data

cachesolve <- function(x, ...) {
    x <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    x
}






