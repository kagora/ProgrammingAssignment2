##----------------------------------------------------------------------------##
## 2014/12/14 kagora                                                          ##
##----------------------------------------------------------------------------##
## Functions cache inverted matrix and return it from the cache if it's
##      requested 2nd time
## For each matrix inversion is performed only once
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
## Input: matrix
## Output : List of objects (functions) 
## Function provides operations to get/set input matrix, and to
##      get/set inverted matrix.
##----------------------------------------------------------------------------##

makeCacheMatrix <- function(x = matrix()) {
    inverseMtrx <- NULL
    
    set <- function(x){
        mtrx <<- x
        inverseMtrx <<- NULL
    }
    
    get <- function() x
    
    setInverseMtrx <- function(inverse) inverseMtrx <<- inverse
    
    getInverseMtrx <- function() inverseMtrx
    
    list(set = set, get = get,
         setInverseMtrx = setInverseMtrx,
         getInverseMtrx = getInverseMtrx)
}

##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
## Input: makeCacheMatrix output, additional parameters for solve() function
## Output: inverted matrix
## If input object contais inverted matrix, then it is returned,
##      otherwise, inversion is performed and returned. It is also saved back 
##      in input object, so next time the computation is not needed.
##----------------------------------------------------------------------------##

cacheSolve <- function(x, ...) {
    inverseMtrx <- x$getInverseMtrx()
    
    if(!is.null(inverseMtrx)) {
        message("getting cached data")
        return(inverseMtrx)
    }
    data <- x$get()
    inverseMtrx <- solve(data, ...)
    x$setInverseMtrx(inverseMtrx)
    inverseMtrx

}
