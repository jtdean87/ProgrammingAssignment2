## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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

cacheSolve <- function(x, ...) {
        nX <- x$getinverted
        if (!is.null(nX()))
        {
                message("cached data")
                return(nX())
        }
        m <- x$get()
        nX <- x$inverted(m)
        nX
}
