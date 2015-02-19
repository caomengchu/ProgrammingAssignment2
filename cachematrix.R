## Below is a solution to solve the inverse a given matrix quickly by combining a cache setting with a traditional
##  inverse calculation. It consist of two functions makeCacheMatrix and cacheSolve.

## makeCacheMatrix is a function of matrix that create several functions and make them a list

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){     
    x<<-y
    inv<-NULL
    
  }
  get<-function()x      
  setinv<-function(inverse){     
    inv<-inverse
  }
  getinv<-function() inv         
  list(set=set,get=get,setinv=setinv,getinv=getinv)  
}


## cacheSolve is a function that return the inverse of a given matrix: it first checks in cache if the matrix
## has already in it.If yes, just get the cached value; if no, do inverse calculation and save it in cache.

cacheSolve <- function(x, ...) {
  inv<-x$getinv()               
  if(!is.null(inv)){            
    message("getting cached data ")
    return(inv)
  }
  data<-x$get()                
  inv<-solve(data)              
  x$setinv(inv)                
  inv
}