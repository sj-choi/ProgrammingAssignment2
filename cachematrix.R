## This script requires a non-singluar matrix.
## This script includes functions, which
## 1) Cache the non-singular matrix
## and
## 2) Caclulate the inverse matrix of the chached matrix.
## UAGES of two funstions are provided at line 50.

## Caching function block: 
## This function chaches the input vector converted to a matrix form. 
makeCacheMatrix <- function(x = matrix()) {
      inv.mat = NULL
      set.mat = function(y) {
            x <<- y
            inv.mat <<- NULL
      }
      get.mat = function() x
      set.inv = function(inv.mat) inv <<- inv.mat 
      get.inv = function() inv.mat
      list(set.mat=set.mat, 
           get.mat=get.mat, 
           set.inv=set.inv, 
           get.inv=get.inv)
}

## Iversion function block:
## This function calculates the inverse matrix of the cached matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv.mat = x$get.inv()
      if (!is.null(inv.mat)){
            # get it from the cache and skips the computation. 
            message("getting cached matrix ...")
            return(inv.mat)
      }
      mat.data = x$get.mat()
      inv.mat = solve(mat.data, ...)
      x$set.inv(inv.mat)  
      return(inv.mat)    
}




######################################################################################################
## USAGE #############################################################################################
######################################################################################################
## 1. Run the corresponding function blocks for "makeCacheMatrix" and "cacheSolve".  
## 2. Generate a vector. 
##          Example) v <- c(1, 1, 0, 1)  
##
## *** NOTE: If the input matrix is singular, the matix may not have its inverse matrix.
##           So, choose a non-singular matrix.
##
## 3. Convert it to a square matrix form using its length. 
##          Example) x <- matrix(v, length(v)/2, length(v)/2)
## 4. Use the function to chache the matrix at sqm. 
##          Example) sqm <- makeCacheMatrix(x)
## 5. Execute the follosing line:
##          cacheSolve(sqm)
##
## *** NOTE: If the input matrix is singular, R generates the following Error message:
##           Error in solve.default(mat.data, ...) : 
##            Lapack routine dgesv: system is exactly singular: U[3,3] = 0
######################################################################################################
######################################################################################################