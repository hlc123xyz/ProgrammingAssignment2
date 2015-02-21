## Put comments here that give an overall description of what your
## functions do:
## the following two function will first cathe a matrix and then compute its inverse.

## Write a short comment describing this function:
## The function below will creates a special "matrix" object that can cathe its inverse.

makeCacheMatrix <- function(mtx = matrix()) {
        inverse<-NULL
        set<-function(x){
                mtx<<-x;
                inverse<<-NULL;
        }
        get<-function() return(mtx)
        setinv<-function(inv) inverse<<-inv;
        getinv<-function() return(inverse);
        return(list(set = set, get=get, setinv=setinv, getinv=getinv))
}


## Write a short comment describing this function:
## the function below will compute the inverse of the special matrix returned by the function 'makeCacheMatrix' above.


cacheSolve <- function(mtx, ...) {
        inverse<-mtx$getinv()
        if(!is.null(inverse)){
                message("getting cached data...")
                return(inverse)
        }
        data<-mtx$get()
        inverse<-solve(data, ...)
        mtx$setinv(inverse)
        return(inverse)
}
