## The aim here is to calculate the inverse of a matrix and to cache the result.  If the inverse is needed again, it can be retrieved from the cache, so that the computation need not be repeated.

## This first function creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x=matrix())
        {
                        i <- NULL
                        set <- function(y)
                                {
                                x <<- y
                                i <<- NULL
                                }
                
                        get <- function() x
                        setinverse <- function(solve) i <<- solve
                        getinverse <- function() i
                        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
                        
                
        }
                       


## This second function computes the inverse of the "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...)
        {
                i <- x$getinverse() # check whether the inverse is present
                if(!is.null(i))
                {
                message("getting cached data")
                return(i)
                }
                data <- x$get()
                i <- solve(data, ...)
                x$setinverse(i)
                i
        }