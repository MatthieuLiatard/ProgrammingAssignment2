## This function will setup the cachable matrix object
#  
makeMatrix <- function(x) {
  m <-NULL
  set <- function(y) {
    x <<- y     ## The matrix is passed to the general environment from the function environment
    m <<- NULL
  }
  get <- function() {x}
  setInv <- function(inversed) m <<- inversed
  getInv <- function() m
  list(set = set,
        get = get, 
        setInv = setInv, 
        getInv = getInv)
}

## This function will properly check the availability of the cached inversed matrix
#  If available, it prints it
#  If not, it computes it and caches it
#  Typical use would be:
#  MatrixObject <- makeMatrix( Matrix)
#  getCachedInverse(MatrixObject)

getCachedInverse <- function(matrixObject, ...) {
  m <- matrixObject$getInv()
  if(!is.null(m)) {
    message("getting cached data");
    return(m)
  }
  
  matData = matrixObject$get()
  m <- solve(matData) #diag(1,dim(matData)[1]))
  message("Computing inverse");
  matrixObject$setInv(m)
  return(m)
}

## HOW TO RUN
#
# Matrix = matrix(sample(9),3,3)  ## Let us pray this isn't singular
# MatrixObject = makeMatrix(Matrix)
# getCachedInverse(MatrixObject) %*% MatrixObject$get()
# getting cached data
# [,1]          [,2] [,3]
# [1,] 1.000000e+00  0.000000e+00    0
# [2,] 0.000000e+00  1.000000e+00    0
# [3,] 8.881784e-16 -4.440892e-16    1
# This is very close to identity matrix