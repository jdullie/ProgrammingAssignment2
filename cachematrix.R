## Matrix inversion is usually a costly computation and there may be some benefit to
##caching the inverse of a matrix rather than compute it repeatedly (there are also 
##alternatives to matrix inversion that we will not discuss here). 
##Your assignment is to write a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL         ##m is a free variable
        set<-function(y){       ## set is a matrix object
                x<<-Y ##x is a matrix set in different environment equals y (a matrix)
                m<<-NULL ## m is Null in a different environment
        }
        get<-function() x  #get is a variable representing a function with no argument and 
        ##x is a matrix
        setmatrix<-function(matrix) m<<-matrix ##setmatrix is variable representing a function and
        ##m is a matrix in a different environment
        getmatrix<-function() m
        list(set=set, get=get, setmatrix=setmatrix,getmatrix=getmatrix)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not 
##changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {  ##cacheSolve is avariable representing a function with the argument 'x' 
        ## Return a matrix that is the inverse of 'x'
        
        m<-x$getmatrix() ## m is a variable representing 'getmatrix' in makeCacheMatrix function above
        if(!is.null(m)) {  ## if the matrix 'm' is not null then display a  message 
                message("getting cached data")
                return (m)
        }
        mat1<-x$get() ## mat1 is a variable representing 'get' in makeCacheMatrix function above
        m<-solve(mat1, ...)  ## is a variable representing an inverse matrix using solve 
        x$setmatrix(m)  # grab 'setmatrix' from makeCacheMatrix function above and insert the inverse matrix 'm'
        m    
}
