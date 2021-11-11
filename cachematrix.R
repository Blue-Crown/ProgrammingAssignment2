## The makeCacheMatrix function creates a special "matrix" (really a list)
## of four sub-functions:
## 1) setmatrix: creates a matrix 'a'
## 2) getmatrix: returns matrix 'a'
## 3) setinv: sets the value to the inverse of matrix 'a', named 'q'
## 4) getinv: returns the value of 'q'



makeCacheMatrix <- function(a = matrix()) {
  q <- NULL     ##creates empty variable 'q',the inverse matrix of 'x'
  setmatrix <- function(b) {
    a <<- b     ##sets 'a' to the value of 'b'
    q <<- NULL  ##resets 'q' to an empty variable
  }
  getmatrix <- function() a  ##returns the matrix 'a'
  setinv <- function(inv) q <<- inv  #sets an inverse matrix 'inv' to value 'q'
  getinv <- function() q ## returns the value of  the inverse matrix 'q'
  list(setmatrix = setmatrix, getmatrix = getmatrix, setinv = setinv, getinv = getinv)
}


## The cacheSolve function returns the value of 'q', 'x's inverse matrix, by 
## first checking if a value has been cached already and, if that exists,
## returning that. If no value has been cached, it will calculate the inverse 
## matrix using solve() and return that.

cacheSolve <- function(a, ...) {
  q <- a$getinv()   ##returns the inverse matrix 'q' from getinv
  if(!is.null(q)) {
    message("Retrieving cached data. BEEP BOOP.")
    return(q)
    ## returns a message if a the inverse matrix q has been previously 
    ## determined, and return cached value of 'q'
  }
  else{
    matrixA <- a$getmatrix()  ##call forward the matrix 'A'
    if(nrow(matrixA) != ncol(matrixA)) {
      message("This matrix is not square, fool.")
    }
    else{
      q <- solve(matrixA, ...) ##calculates the inverse of matrix 'A', and calls it 'q'
      a$setinv(q) ##set the inverse of 'A' as 'q'
      message("Inverse matrix successfully calculated! BEEP BOOP.")
      return(q)
    }
  }
}

