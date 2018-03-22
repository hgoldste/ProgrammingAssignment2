##Matrix inversion is a costly computation and there are may benefits to caching the inverse of a matrix rather than computing it repeatedly. 
##This pair of functions is created in order to cache the iverse of a matrix. 
##This first function makeCacheMatrix creates a special matrix object that can cache its inverse. 

makeCacheMatrix <- function(x= matrix()){
inverse <- NULL
set <- function(y){ 
	x <<- y 
	inverse <<- NULL
} 
get <- function()x 
setinverse <- function(MatrixInv) inverse <<- MatrixInv
getinverse <- function() inverse
list(set=set, get=get, 
setinverse=setinverse, 
getinverse=getinverse)
}

##This next function is created to compute the inverse of the special matrix that was created/returned from the previous function makeCacheMatrix presented above. 
##Therefore, this function cacheSolve computes or retrieves the inverse from cache. 

cacheSolve <- function(x, ...){
inverse <- x$getinverse()
if (!is.null(inverse)){
	message("getting cached data")
	return(inverse)
}
matx <- x$get()
inverse <- solve(matx, ...) 
x$setinverse(inverse)
inverse
}
