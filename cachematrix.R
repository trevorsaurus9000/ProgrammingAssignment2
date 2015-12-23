##Trevor Harris
##12/23/2015
##Coursera
##R_Programming Class 035



##FUNCTION PURPOSE: This is my test function used to both understand and test the
##makeCacheMatrix() and cacheSolve() functions.
##EXPECTED INPUTS: <null>
##EXPECTED OUTPUTs: Hopefully an identity matrix.
testInverseCachedMatrix <- function() {
        
        ##create a 4x4 test matrix
        testMatrix <- matrix(rnorm(16),4,4)
        print("Our test matrix: ")
        print(testMatrix)
        
        x <- makeCacheMatrix(testMatrix)
        
        message("Calling cacheSolve() for the first time...")
        cacheSolve(x)
        print("Our inverse matrix: ")
        print(x$getInverseMatrix())
        
        message("Calling cacheSolve() for the second time...")
        cacheSolve(x)
        print("Our inverse matrix: ")
        print(x$getInverseMatrix())
        
        print("Did we get an identity matrix when multiplying our matrix by its inverse?")
        print("Our identity matrix: ")
        inverseTestMatrix <- x$getInverseMatrix()
        identityMatrix <- testMatrix %*% inverseTestMatrix
        identityMatrix <- round(identityMatrix,1)
        print(identityMatrix)
        
}



##FUNCTION PURPOSE: Create a 'special' matrix object that can cache its inverse.
##subfunction setMatrix() stores the input matrix within the function.
##subfunction getMatrix() retrieves the stored matrix.
##subfunction setInverseMatrix() stores the inverse of the input matrix within the function.
##subfunction getInverseMatrix() retrieves the stored inverse matrix. 
##EXPECTED INPUT: A populated, invertable (square) matrix.  If an input is not
##provided, an empty matrix will be generated.
##EXPECTED OUTPUT: A 'special' matrix object that contains both the input matrix (if provided),
##and subfunctions that can be applied against that matrix.
makeCacheMatrix <- function(cachedMatrix = matrix()) {
        
        ##Create a null object that will eventually become our cached inverse matrix.
        cachedInverseMatrix <- NULL
        
        ##Cache the input matrix.
        setMatrix <- function(inputMatrix) {
                cachedMatrix <<- inputMatrix
                cachedInverseMatrix <<- NULL
        }
        
        ##Cache the inverse of our input matrix.
        ##We can do this using the solve() function since we're gaurenteed square matrices.
        setInverseMatrix <- function(solve) cachedInverseMatrix <<- solve
        
        ##Retrieve the cached input matrix.
        getMatrix <- function() cachedMatrix
        
        ##Retrieve the inverse of our input matrix.
        getInverseMatrix <- function() cachedInverseMatrix
        
        ##Return our 'special' matrix object, which is really just a list of subfunctions.
        list(setMatrix = setMatrix,
             getMatrix = getMatrix,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)

}



##FUNCTION PURPOSE: Computes the inverse of the matrix returned by makeCacheMatrix().
##This is done by first checking to see if the inverse of the input matrix has already been
##computed and cached.  If it has, the stored version of that matrix's inverse will be returned.
##Otherwise, we'll spend cycles generating the inverse matrix.
##EXPECTED INPUT: A 'special' matrix object returned by makeCacheMatrix().
##EXPECTED OUTPUT: An inverse matrix of the input matrix.
cacheSolve <- function(ourSpecialObject, ...) {

        ##Check to see if an inverse matrix has already been cached.
        ##If it has, retrieve it and exit the function.
        ourInverseMatrix <- ourSpecialObject$getInverseMatrix()
        if(!is.null(ourInverseMatrix)) {
                message("Retrieving cached inverse matrix...")
                return(ourInverseMatrix)
        }
        
        ##If a cached inverse matrix does not already exist, generate it using the solve() function.
        message("Inverse matrix not in cache.  Generating...")
        ourMatrix <- ourSpecialObject$getMatrix()
        ourInverseMatrix <- solve(ourMatrix, ...)
        
        ##Cache our new inverse matrix for next time.
        ourSpecialObject$setInverseMatrix(ourInverseMatrix)
        
        ##Return our inverse matrix.
        ourInverseMatrix
}
