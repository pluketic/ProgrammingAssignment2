## makeCacheMatrix creates a special matrix object, and then cacheSolve calculates the inverse of the matrix.
## If the matrix inverse has already been calculated, it will instead find it in the cache and return it, and not calculate it again.

## makeCacheMatrix function defines methods for inversing & caching inverse of the input matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse_x <- NULL                                   # sets the value of local vector inverse_x to NULL (placeholder for inverse matrix)
    set<-function(y){                                   # define set() function which 
        x <<- y                                         # caches input matrix
        inverse_x <<- NULL                              # sets the value of cached vector inverse_x to NULL
    }
    get<-function() x                                   # define get() function which returns input matrix
    setinverse<-function(solve) inverse_x <<- solve     # define setmatrix() function which caches variable m and assigns to it inverse of the input matrix
    getinverse<-function() inverse_x                    # define getmatrix() function which returns cached variable inverse_x (inverse of the input matrix)
    list(set=set, get=get,                              # define the list of external functions and their local definition names
         setinverse=setinverse,
         getinverse=getinverse)
}


## cacheSolve function returns inverse matrix first by checking whther an
## inverse matrix already exists in cache, if not then it calculates & caches it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse_x <- x$getinverse()                         # defines local vector inverse_x and assigns it a cached vector (inverse of the input matrix)
    if(!is.null(inverse_x)){                            # if there is an cached inverse matrix inverse_x available in the cache
        print("getting cached inverse matrix")          # notify me about existing cached vector
        return(inverse_x)                               # and make function return cached vector inverse_x (inverse of the input matrix)
    } else {                                            # else
        print("getting new data")                       # notify me that the is no cached data available, so that the function has to make inverse matrix from scratch
        matrix <- x$get()                               # run the get() function to get the input matrix
        inverse_x <- solve(matrix, ...)                 # define local variable inverse_x and assign it a value of inverse of the input matrix
        x$setinverse(inverse_x)                         # run the setmatrix function on the input matrix to cache it
        return(inverse_x)                               # return inverse_x
    }
}
