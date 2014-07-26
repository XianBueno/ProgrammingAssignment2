## makeCacheMatrix creates a special object that can cache the inverses of
## matrices (a computationally expensive calculation), and cacheSolve allows
## us to quickly return the inverted matrix if the calculation was done recently


## For makeCacheMatrix, the input x must be a matrix. Once we pass the matrix
## to the function, makeCacheMatrix will create a "special object" which
## is effectively a list of functions with some internal values (a class). 
## Suppose that we use this function on some square matrix A by saying:
##
##   M <- makeCacheMatrix(A)
##
## Then M will be equiped with the values x=A and inv=NULL as well as with
## the functions set,get,setinv,getinv. When I say that M is equipped with the 
## functions, I mean that M$get() or M$setinv() are commands that can be used 
## that will interact with M's internal variables (x and inv). 
##
## If we wish to see the internal variables we can do so with M$get() or 
## M$getinv() for the matrix and inverse (resp.). If we wish to change the x or 
## inv we can use M$set() and M$setinv() (resp.). We can specifically use 
## M$set(A) followed by M$setinv(solve(A)) to store A and cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL  #This is included to reset inv whenever we set x
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## For this function, the input x must be an object created by makeCacheMatrix.
## Once it has this object, it will invert the matrix stored in x
## (the stored matrix can be called with x$get). If the inverse of the matrix
## has been cached within x, then it will simply call it up with x$getinv().
## If the inverse has not been cached, then it will compute it and then cache 
## with x$setinv().

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    ## Check if there is a cached inverse within x. 
    ## If so, don't use solve() and use the cached value instead.
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## We resort to calculating the inverse with solve() if inv=NULL
    matrix <- x$get()
    inv <- solve(matrix, ...)
    ## We make sure to store/cache the calculated inverse in x
    x$setinv(inv)
    inv
}
