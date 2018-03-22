##Matrix inversion is a costly computation and there are may benefits to caching the inverse of a matrix rather than computing it repeatedly. 
##This pair of functions is created in order to cache the iverse of a matrix. 
##This first function makeCacheMatrix creates a special matrix object that can cache its inverse. 

makeCacheMatrix <- function(x= matrix()){
inv <- NULL
set <- function(y){ 
	x <<- y 
	inv <<- NULL
} 
get <- function()x 
setInverse <- function(MatrixInv) inv <<- MatrixInv
getInverse <- function() inv
list(set=set, get=get, 
setInverse=setInverse, 
getInverse=getInverse)
}

##This next function is created to compute the inverse of the special matrix that was created/returned from the previous function makeCacheMatrix presented above. 
##Therefore, this function cacheSolve computes or retrieves the inverse from cache. 

cacheSolve <- function(x, ...){
inv <- x$getInverse()
if (!is.null(inv)){
	message("getting cached data")
	return(inv)
}
matx <- x$get()
inv <- solve(matx, ...) 
x$setInverse(inv)
inv
}
