## Functins to cache the inverse of a matrix

## This function creates a special matrix with four functions (methods)

makeCacheMatrix <- function(mtx = matrix()) {
        ##This function creates a special matrix with four functions (methods)
        
        inv <- NULL
        
        ## Sets the special matrix
        set <- function(matrix){
                mtx <<- matrix
                inv <<- NULL
        }
        
        ## Gets the special matrix
        get <- function(){
                mtx
        }
        
        ## Sets the inverse of the matrix
        setInverse <- function(inverse){
                inv <<-  inverse
        }
        
        ## Gets the inverse of the matrix
        getInverse <- function(){
                inv
        }
        
        ## Return value is a list with set, get, setInverse and getInverse
        list(set = set, get = get, setInverse =  setInverse, getInverse = getInverse)
}


## The following function calculates the inverse of the special matrix created 
## with the makeCacheMatrix function. However, it first checks to see if the
## inverse has already been calculated. If so, it gets the inversed 
## from the cache and skips the computation. Otherwise, it calculates the 
## inverse of the matrix and sets the value of the inverse in the cache via 
## the setInverse function of the special matrix

cacheSolve <- function(x, ...) {
        
        ## Returns the inverse of the special matrix x
        mtx <- x$getInverse()
        
        ## Returns the inverse if it has already been calculated
        if(!is.null(mtx)){
                message("getting cached data")
                return(mtx)
        }
        
        ## Gets the matrix from the special matrix x
        data <- x$get()
        
        ## Calculates the inverse using the solve function and matrix multiplication
        mtx <- solve(data) %*% data
        
        ## sets the inverse in the special matrix x
        x$setInverse(mtx)
        
        ## Return the inverse of the matrix
        mtx
}
