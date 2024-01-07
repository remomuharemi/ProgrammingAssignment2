## Put comments here that give an overall description of what your
## functions do
## The first function creates a list containing a matrix, 
   #its inverse and the methods for setting and getting them.
## Assuming the matrix supplied to the first function is invertible, the second
   #function checks if the inverse is already cached, gets and returns it 
   #if so, and if not, computes and returns it.

##This function creates a list containing an invertible matrix, its inverse,
  #and methods for getting and setting such matrices.
makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<- function() x
        setinverse<- function(inverse) inv<<-inverse
        getinverse<-function() inv
        list(set=set,get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}
X<-makeCacheMatrix(data)

## This function only operates on outputs of the previous function.
## It uses the methods and matrices created by makeCacheMatrix().
## It first checks if we have already an inverse of the matrix cached.
## If so, it gets such cached inverse matrix (via getinverse())and returns it.
## If not, it proceeds to get the original matrix and invert it using solve().
## Using this it updates (via setinverse()) the inverse item in the list. 
## Lastly, it returns the inverse of the matrix.
cacheSolve <- function(x, ...) {
        inv<-x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data<-x$get()
        inv<-solve(data,...)
        x$setinverse(inv)
        inv
}
