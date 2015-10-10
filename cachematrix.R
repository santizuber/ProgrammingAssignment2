## This functions create a special matrix that can store the computed value of its inverse
## The inverse can be calculated once with cacheSolve, and remain stored to be accesed again 
## until the matrix changes, without need of further calculation.

## makeCacheMatrix creates an object that can store the value of a matrix and its inverse
## It creates a list of functions which are used to acces and write the stored data

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL #Sets default value for inverse
        set <- function(y) { #Sets new matrix value and resets inverse value to NULL
                x <<- matrix(y)
                inverse <<- NULL
        }
        get <- function() x #Returns matrix 
        setinverse <- function(solve) inverse <<- solve #Used by cacheSolve to store the inverse value when calculated
        getinverse <- function() inverse #Used by cacheSolve to retrieve the inverse value when cached
        list(set = set, get = get, #List of complementary functions of matrix
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve uses the functions created by makeCacheMatrix to retrieve the stored inverse if
## available. If data is not available, it calculates the inverse of the matrix and caches it.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse() #Retrieves cached information from especial matrix x
        if(!is.null(inverse)) { #If stored data available, returns stored value
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}

