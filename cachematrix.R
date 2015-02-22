## These functions will cache and compute the inverse of a matrix
##
## Computing the inverse of a square matrix is done using the solve function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverseMatrix<-function(solve) m<<- solve
        getInverseMatrix<-function() m
        return(list(set=set, get=get,
             setInverseMatrix=setInverseMatrix,
             getInverseMatrix=getInverseMatrix))
        
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       
         m <- x$getInverseMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setInverseMatrix(m)
        return(m)                
}