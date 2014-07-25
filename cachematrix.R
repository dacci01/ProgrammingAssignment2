## R Programming Assignment #2
## July 2014

## This assignment develops a deeper understanding of lexical scoping in R. The
## functions operate on a square matrix, makeCacheMatrix() creates a list of 
## functions which create and cache the matrix inverse. 
## cacheSolve() computes the inverse returned by the makeCacheMatrix object
## if it is not already stored in that environment.

## makeCacheMatrix() creates and caches a square, invertible, matrix in a local
## environment that is retrievable from the cacheSolve funtion

makeCacheMatrix <- function(MyMatrix = matrix(), MyMatrixInv = NULL) {
        #
        setMatrix <- function(y) {
                MyMatrix <<- y
                MyMatrixInv <<- NULL
        }
#
        getMatrix <- function() MyMatrix
#
        setInv <- function(Inv) MyMatrixInv <<- Inv
#
        getInv <- function() MyMatrixInv
#
        list(setmat = setMatrix, getmat = getMatrix,setinv = setInv,
             getinv = getInv)
}

## cacheSolve() computes, using solve(), or returns the cached value of a square
## matrix inverse returned by the makeCacheMatrix object.

cacheSolve <- function(MyMatrix, ...) {
        ## Return a matrix that is the inverse of 'MyMatrix'
        MyMatrixInv <- MyMatrix$getinv()
        if(!is.null(MyMatrixInv)) {
                message("Getting cached value")
                return(MyMatrixInv)
        }
        mdata <- MyMatrix$getmat()
        # check to see that matrix is square
        chk_sq <- dim(mdata)
        if(chk_sq[1] == chk_sq[2]) {
                MyMatrixInv <- solve(mdata, ...)
        } else {
                message("\nNot a square matrix !\nInverse set to")
        }

        MyMatrix$setinv(MyMatrixInv)
        return(MyMatrixInv)
}
