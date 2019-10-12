## This pair of functions cache the inverse of a given matrix, so who ever use
## this don't have to compute the inverse of the matrix over and over again

## This function contain four small functions and two data objects, which finish
## the job of getting and setting the matrix and its inverse and store them

makeCacheMatrix <- function(x = matrix()) {
    if(nrow(x) == ncol(x) && det(x) != 0){
        m <- NULL
        set <- function(y){
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set=set, get = get, setInverse = setInverse, getInverse = getInverse)
    }else{
        massage("This matrix is irreversible")
    }
}


## Combining with makeCacheMatrix function, this function allows people to 
## calculate and store the inverse of the input argument if it is of type
## makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
