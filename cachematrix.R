makeMatrix <- function(x) {  
  m <-NULL  
  set <- function(y) {   
    x <<- y   
    m <<- NULL  
  }  
  get <- function() x
  setInv <- function(inversed) m <<- inversed
  getInv <- function() m  
  list(set = set, 
        get = get, 
        setInv = setInv, 
        getInv = getInv)
}

getCachedInverse <- function(matrixObject, ...) {  
  m <- matrixObject$getInv()  
  if(!is.null(m)) {    
    message("getting cached data")   
    return(m)  
  }  
  
  matData = matrixObject$get() 
  m <- solve(matData, diag(1,dim(matData)[1]))  
  message("Computing inverse")  
  matrixObject$setInv(m)  m
}