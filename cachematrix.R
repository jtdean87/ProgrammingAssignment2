## Contains two functions that can be used to create a special matrix
## that can return and cache the inverted matrix


## makeCacheMatrix creates a special matrix that can cache its invert

makeCacheMatrix <- function(x = matrix()) {
        iX <- NULL
        set <- function(y){
                x <<- y
                iX <<- NULL
        }
        get <- function(){
                x
        }
        inverted <- function(x) {
                iX <<- solve(x)
                iX
        }
        getinverted <- function(){
                iX
        }
        list(
                set=set,
                inverted = inverted, 
                get=get,
                getinverted = getinverted
        )
}

##cacheSolve returns the invert of the special matrix created in makeCacheMatrix. 
##Works by first checking cache and if no result is found will calculate invert
##and return it.

cacheSolve <- function(x, ...) {
        nX <- x$getinverted
        if (!is.null(nX()))
        {
                message("pulling from cache")
                return(nX())
        }
        m <- x$get()
        nX <- x$inverted(m)
        nX
}
