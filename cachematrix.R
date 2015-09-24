## Two functions working together creating a cache for storing   
## an already computed inverted matrix. This will reduce the amount 
## of computations needed.

## The first function, `makeCacheMatrix` creates a 
#list containing a function to:
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverted matrix
# 4.  get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        #initialize
        cache <- NULL
        
        #create the matrix (in the working environment?)
        set <- function(y){
                x <<- y
                cache <<- NULL
        }
        
        get <- function() x
        # invert the matrix and store in cache
        setMatrix <- function(inverse) cache <<- inverse
        getMatrix <- function() cache
        
        list(set = set, get = get,
             setMatrix = setMatrix,
             getMatrix = getMatrix)
}


## The 'cacheSolve' function returns the inverse of a matrix
## either from the 'makeCacheMatrix' function or by computing it

cacheSolve <- function(x, ...) {
        # get the contents of the cache
        cache <- x$getMatrix()
        # checking for the existence of a result,
        # if one is found return the cached result
        if(!is.null(cache)){
                message("Getting cached data.")
                return(cache)
        }
        
        # if not, use 'solve' function to compute inversion
        matrix <- x$get()
        cache <- solve(matrix,...)
        x$setMatrix(cache)
        cache
        ## Return a matrix that is the inverse of 'x'
}
