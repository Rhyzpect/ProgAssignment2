## It takes a while to calculate the inverse of a large matrix and 
## of a large set of matrices.  This is time consuming and takes up a lot of 
## memory.  For efficiency, it is recommended that the matrices are
## cached especially if there are identical matrices in the set. 

##The following function stores matrices and inverse matrices.
## The makeCacheMatrix function stores the inverse of matrices so that
## efficient calculation is achieved. The function stores four functions, i.e.,
## MatrixStore, StoredMatrix, InverseMatrix and StoredInverse.

makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
                MatrixStore <- function(y){ 
        
                # function stores inputted matrix
                
                        x <<- y
                        inv <<- NULL 
                        }
                StoredMatrix <- function() x
                InverseMatrix <- function(inverse) inv <<- inverse
                StoredInverse <- function() inv 
                
                # function stores the inverse matrix
                # The function then makes a list of stored matrices
                list(MatrixStore = MatrixStore,
                     StoredMatrix = StoredMatrix,
                     StoredInverse=StoredInverse,
                     InverseMatrix=InverseMatrix)
                }

## cacheSolve function first checked for StoredInverse.  
## If the function finds what it is looking for in the cached data, 
## it returns the inverse matrix.  However if not, it calculates the inverse
## of the matrix using solve().
## Then it returns a matrix that is the inverse of 'x'

cacheSolve <- function(x,...) {
        
        inv <- x$StoredInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$StoredMatrix()
        inv <- solve(data,...)
        x$InverseMatrix(inv)
        inv
}
