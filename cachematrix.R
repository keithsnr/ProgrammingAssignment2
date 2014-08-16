## The cachesolve function retrieves a matrix and checks to see if it is 
## inverted. 
## If it is inverted it returns the matrix. If it is not,
## it calls a function on the matrix,inverts it and then return.

## This function creates a matrix inverts and caches it.

makeCacheMatrix <- function(x = matrix()){
        mat <- NULL  ## 
        set <- function(y)
        {
                x <<- y
                mat <<- NULL
        }
        get <- function() x 
        setmatrix <- function(solve) mat <<- solve 
        getmatrix <- function() mat
        list(set = set, get = get, setmatrix = setmatrix,                           
             getmatrix = getmatrix)           
        
}


## This function returns an inverted matrix.  

cacheSolve <- function(x, ...) {
        
        mat <- x$getmatrix()
        if (!is.null(mat)) { ## check if matrix is inverted
                message("retrieving cached data")
                return(mat)
        }
        d_results <- x$get()
        mat <- solve(d_results, ...)
        x$setmatrix(mat)
        mat               
}