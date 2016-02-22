##  Programming Assignment #2 (Week 3)
##  Lexical Scoping
##  makeCacheMatrix.R


## This function is used to cache the inverse of a matrix to save time by avoiding
##  recalculation of the value

## this function will accomplish the following steps
## 1) set the value of the vector
## 2) get the value of the vector
## 3) set the value of the mean
## 4) get the value of the mean


## Create a list that will hold the info on the inverted matrix 

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL                             ## clears any cached matrix
  set <- function(y) {                  ## set the value of matrix
    x <<- y                             ## magical assignment to parent environment?
    m <<- NULL
  }
  get <- function() x
  setinvmatrix <- function(invmatrix) m <<- invmatrix
  getinvmatrix <- function() m          ## function to retreive value of inverted matrix
  
  list(set = set, get = get,            ## creates a list of all functions
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)
}


##  Programming Assignment #2 (Week 3)
##  Lexical Scoping
##  cachesolve.R

cachesolve <- function(x, ...) {        ## returns the inverted matrix
  m <- x$getinvmatrix()                 ## returns the cached matrix
  if(!is.null(m)) {                     ## checking for already cached matrix
    message("getting cached data")
    return(m)
  }
  data <- x$get() 
  m <- solve(data, ...)                ## use solve function
  x$setinvmatrix(m)                    ## persist inverse in cache
  m                                    ## return inverse of matrix
  
  
}