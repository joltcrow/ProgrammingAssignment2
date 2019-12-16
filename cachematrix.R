## Put comments here that give an overall description of what your
## functions do


## This variables will be use as the cache environment 
mat <- NULL
inv <- NULL

## Write a short comment describing this function
## makeCacheMatrix store a list of function to set/get the matrix/inverse in the cache environment
makeCacheMatrix <- function(){
        set <- function(y){
                mat <<- y
                inv <<- NULL
        }
        get <- function() mat
        setinv <- function(smat) inv <<- smat
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function
## cacheSolve check if we already have the inverse of the in cache (if not compute it) and return it
cacheSolve <- function(x){
        smat <- x$getinv()
        if(!is.null(smat)){
                cat("return cached inverse\n")
                return(smat)
        }else{
                cat("return computed inverse\n")
                smat = solve(x$get())
                x$setinv(smat)
                return(smat)
        }
}



## Example of how to use these functions 
X<-matrix(runif(64), 8)
obj_makeMat = makeCacheMatrix()
obj_makeMat$set(x)
cacheSolve(obj_makeMat)
##