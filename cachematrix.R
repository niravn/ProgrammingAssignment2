## To set and get the matrix as well as its inverse

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
       x<<-y
       i<<-NULL
  }
  get<-function() x
  setMat<-function(solve) i<<-solve
  getMat<-function() i
  list(set=set, get=get, 
       setMat=setMat,
       getMat=getMat)

}

## To cache the inversion of a square matrix

cacheSolve <- function(x=matrix(), ...) {
        i<-x$getMat()
        if(!is.null(i)){
          message("getting cached data")
          return(i)
        }
        data<-x$get()
        i<-solve(data, ...)
        x$setMat(i)
        i
}
