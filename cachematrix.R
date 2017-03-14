## Programming Assigment #2 ()
## Forked from: https://github.com/rdpeng/ProgrammingAssignment2
##
##
## Reinaldo Giudici
##
## The following two code allows to create a matrix object that can cache the
## inverse of the passed matrix once it's calculated by the cacheSolve method.
## This allows for code using this new matrix type, to quickly get the result
## from memory, and only calculate the inverse once.

# Example:
# > m <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
#
# > m$getInverse()
# NULL
#
# Result is NULL because the inverse has not been calculated.
#
# > s <- cacheSolve(m)
# now s has the inverse of the matrix
# > s
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
#
# And so does the matrix object:
#
# > m$getInverse()
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
#
#

## makeCacheMatrix: This function creates a special "matrix" object that can
## cache its inverse.
##
## it creates a new matrix object that exposes the following methods:
## set(m)        - Sets the matrix value, this can be used to "replace" the
##                 matrix that was used when originally called.
## get()         - Returns the matrix value stored in this object
## setInverse(m) - Stores in the cache the value passed
## getInverse()  - Retrieves from the cache the stored value for the
##                 inverse of the matrix.
##
##

makeCacheMatrix <- function(m1 = matrix()) {
   mInv <- NULL

   # Set the original matrix object locally
   set <- function(m2)  {
      m1 <<- m2
      ## clear the cache as we store the new matrix value
      mInv <<- NULL
   }

   # returns the original matrix object
   get <- function()  {
      m1
   }

   # called to store the inverse of the matrix,
   # this is assigned using the <<- operator so is available
   # outside of this function.
   setInverse <- function(inverse_matrix)  {
      mInv <<- inverse_matrix
   }

   # returns the stored value for the matrix inverse.
   getInverse <- function () {
      mInv
   }

   # list to expose the "special" matrix object functions
   list(
      set = set,
      get = get,
      setInverse = setInverse,
      getInverse = getInverse
   )

}

## This function "fills" the matrix object cache, by checking if the
## inverse has been set before.
## if it has been set then the value is used,
## if not then the actual "solve(matrix)" function  is called, the inverse is
## calculated, and the value is stored in the matrix object cache.
## successive calls to this function will use the cahed value.
##
## This function returns the inverse of the matrix passed
cacheSolve <- function(m, ...) {

   # attempt to get the inverse value from the cached
   inverse <- m$getInverse()

   # If available from the getInverse, then return.
   if(!is.null(inverse)) {
      inverse
   } else {
      # else calculate cache and return
      matrix <- m$get()
      inverse <- solve(matrix, ...)
      m$setInverse(inverse)
      inverse
   }
}
