## Function responsible for constructing cache object for a specified matrix 
## and its respective inverse matrix, when extracted

makeCacheMatrix <- function(x = matrix()) {
    storedInverseMatrix <- NULL
    set <- function(matrix) {
        x <<- matrixValue
        storedInverseMatrix <<- NULL
    }
    get <- function() x
    setInverseMatrix <- function(inverseMatrix) storedInverseMatrix <<- inverseMatrix
    getInverseMatrix <- function() storedInverseMatrix
    list(set = set, 
         get = get, 
         setInverseMatrix = setInverseMatrix, 
         getInverseMatrix = getInverseMatrix)
}


## Function responsible for receiving a cache object and returning the inverse matrix
## from the matrix stored in the cache object

cacheSolve <- function(x, ...) {
    storedInverseMatrix <- x$getInverseMatrix()
    if(!is.null(storedInverseMatrix)) {
        message("Getting cached data")
        return(storedInverseMatrix)
    }
    data <- x$get()
    inverseMatrix <- tryCatch.W.E(solve(data, ...))
    if (is.matrix(inverseMatrix$value)) {
        storedInverseMatrix <- inverseMatrix$value
        x$setInverseMatrix(storedInverseMatrix)
    }
    else {
        message("The matrix is non-invertible")
    }
    storedInverseMatrix
}
