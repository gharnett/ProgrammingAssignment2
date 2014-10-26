## The pair of functions here enable the caching of a matrix and its inverse in one object.
## The inverse is not calculated until first requested; subsequent requests return the cached value.
## The object admits a method to reset the matrix held by the object to a new value.
## After resetting, the inverse is again not calculated until requested.

# The function makeCacheMatrix takes a presumed-invertible matrix and
# returns a list of four items.
# Two of the items are empty-argument functions that return the current values of the matrix
# and its inverse (which latter is initially null).
# Two of the items are functions which enable the reset of the values of the matrix and its inverse.
makeCacheMatrix <- function(m = matrix()) {
	
    # The value of minv is to be the inverse of the current value of variable m,
    # which takes values of an invertible square matrix.
    # It's what x$currentInverse() will return if x is a value of makeCacheMatrix.
    # Also this establishes minv as a member of the environment associated with x.
    minv <- NULL
    
    resetMatrixF <- function(newm) {
        # When resetMatrixF is called by x$resetMatrix(arg),
        # the assignment syntax <-- imposes a lookup
        #--outside the resetMatrixF environment--for the "home" of variable m,
        # which will be the environment associated with x.
        m <<- newm
        minv <<- NULL
    }
    
    # setInverseF should be called with the inverse of the current value of the matrix m
    # the inverse being computed elsewhere.
    setInverseF <- function(shouldBeInverse) minv <<- shouldBeInverse
    matrixValueF <- function() m
    inverseValueF <- function() minv
    
    # The returned list, say x, has the following four elements.
    # The first two reference the functions that reset values in the environment of x.
    # The second two reference the constant functions whose
    # values are respectively the matrix and its inverse (possibly null).
    list(
	resetMatrix = resetMatrixF,
	setInverse = setInverseF,
	currentMatrix = matrixValueF,
	currentInverse = inverseValueF
    )
}

## The function cacheSolve takes a value x of makeCacheMatrix
## and returns an ordinary matrix---the inverse of the matrix carried by x, namely x$currentMatrix().
## If cacheSolve has been previously called on x carrying the same internal values,
## then it simply returns the cached value of the inverse.
## Otherwise, it calculates the inverse, caches it, as well as returning it.
## So there are side effects of this thing!
cacheSolve <- function(x, ...) {
    xinv <- x$currentInverse()
    if(!is.null(xinv)) {
        message("getting cached inverse")
        return(xinv)
    }
    # otherwise
    xinv <- solve(x$currentMatrix())   # Request the matrix itself and calculate its inverse.
    x$setInverse(xinv)  # Ask x to set its currentInverse value to be xinv.
    xinv  # BTW return the inverse that was calculated.
}