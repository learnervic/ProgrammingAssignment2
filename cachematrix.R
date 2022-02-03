##This program is a pair of functions that cache the inverse of a matrix

## This program creates a matrix object that can chache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() {x}
  setinv<-function(inverse){
    inv<<-inverse}
  getinv<-function(){
   inverse}
  list(set = set, get=get, 
       setinv=setinv, 
       getinv=getinv)
}


##This function computes the inverse of the special matrix, if the inverse
##has already been calculated then the cachsolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  inv<-x$getinv()
  if(!is.null(inv)){
    message("getting cached data - the inverse of the matrix")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  inv}
