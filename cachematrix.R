## This is the solution of the excercise proposed in Coursera R Programming course
## Following functions will cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.
## It creates a list containing a function to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 4.get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
    ## m stores the cached value  
    m <- NULL
    
    # create the matrix in the working  
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # get the value of x matrix  
    get <- function() x
    
    # m stores the inverse of the matrix
    setInverse <- function(inverse) m <<- inverse
    
    # get the inverted matrix
    getInverse <- function() m
    
    #return the list
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (
## and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # m stores the inverse
    m <- x$getinverse()
    
    # if m is not null
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
     }

     # get the matrix, find the inverse and cache it 
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     
     # return the value
     m
  
}
