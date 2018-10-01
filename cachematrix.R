## Put comments here that give an overall description of what your 
## functions do 
## Write a short comment describing this function 

makeCacheMatrix <- function(m = matrix()) { 

  inverse = NULL;
    set<-function(n){
      m<<-n
      inverse<<-NULL
}

    get<-function()
      m
    
    setinv<-function(inv) 
      inverse<<-inv
    
    getinv<-function() 
      inverse
    
    list(set=set,get=get,setinv=setinv,getinv=getinv)

} 

## Write a short comment describing this function 
cacheSolve <- function(x, ...) { 
  
  inverse<-x$getinv()
  
  if(!is.null(inverse)) { 

    message("getting cached data.") 
    return(inverse) 
  
  }
  
  d <- x$get() 
  
  inverse <- solve(d) 
  
  x$setinv(inverse) 
  
  inverse
  
} 
