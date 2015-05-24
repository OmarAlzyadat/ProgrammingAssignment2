## this function will take a matrix and calculate its inverse and store it another matrix. 

makeCacheMatrix <- function(mtx = matrix()) 
{
        ## giving the matrix an initial value set to null
        inverse <- NULL
        
        ## defining a function 
        set <- function(x) 
        {
                mtx <<- x;
                inverse <<- NULL;
        }
        
        get <- function() return(mtx);
        setinv <- function(inv) inverse <<- inv;
        getinv <- function() return(inverse);
        return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache. if the matrix has been changed 
## or the the inverse has not been calculated it will calculate the inverse of the given matrix

cacheSolve <- function(mtx, ...) 
{
        inverse <- mtx$getinv()
        
        ## checking if the iverse is already stored in the special matrix
        if(!is.null(inverse)) ## the inverse is stored 
        {
                message("Getting cached data...")
                return(inverse)
        }
        
        data <- mtx$get()
        ## solving the matrix inverse and assigning it to a matrix called Inverse
        invserse <- solve(data, ...)
        mtx$setinv(inverse)
        ##returning the matrix named Inverse
        return(inverse)
}
