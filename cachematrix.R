# H-RM coded & tested @ 10-11Oct2015


## The functions makeCacheMatrix() and cacheSolve()
# enable the inverse of an invertible matrix to be cached for retrieval without unnecessary recalculation.


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# ----Use with cacheSolve() function below.
# The special "matrix" is essentially a list containing
# a function set() to set the value of the matrix
# a 2nd function get() to get the value of the matrix
# a 3rd function setMatInv() to set the value of the matrix's inverse
# a 4th function getMatInv() to get the value of the matrix's inverse


makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL

      set <- function(y) {
            x <<- y
            inv <<- NULL
      }

      get <- function() x

      setMatInv <- function(matrixInv) inv <<- matrixInv

      getMatInv <- function() inv

      list(set = set, get = get,
           setMatInv = setMatInv,
           getMatInv = getMatInv)
}



## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix() function above.
# If the inverse has already been calculated (and the matrix has not changed),
# then cacheSolve() would retrieve the inverse from the cache via getMatInv() function and skips the computation.
# Otherwise, it computes the inverse of the special "matrix" data and sets the value of the matrix-inverse in the cache
# via the setMatInv() function.


cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getMatInv()

      if(!is.null(inv)) {
            message("getting cached matrix-inverse data")
            return(inv)
      }

      data <- x$get()

      inv <- solve(data, ...)

      x$setMatInv(inv)

      inv
}



### TEST functions & notes for personal reference ############################################
#
# Mat <- matrix(seq(2,8, by=2),nrow = 2, ncol = 2)
# Mat
# # [,1] [,2]
# # [1,]    2    6
# # [2,]    4    8
#
# mMat <- makeCacheMatrix(Mat)
# mMat
# # $set
# # function (y)
# # {
# #       x <<- y
# #       inv <<- NULL
# # }
# # <environment: 0x10b325098>
# #
# #       $get
# # function ()
# #       x
# # <environment: 0x10b325098>
# #
# #       $setMatInv
# # function (matrixInv)
# #       inv <<- matrixInv
# # <environment: 0x10b325098>
# #
# #       $getMatInv
# # function ()
# #       inv
# # <environment: 0x10b325098>
#
# mMat$get()
# # [,1] [,2]
# # [1,]    2    6
# # [2,]    4    8
#
# mMat$getMatInv()
# # NULL
#
# cSmMat <- cacheSolve(mMat)
# cSmMat
# # [,1]  [,2]
# # [1,] -1.0  0.75
# # [2,]  0.5 -0.25
#
# mMat$getMatInv()
# # [,1]  [,2]
# # [1,] -1.0  0.75
# # [2,]  0.5 -0.25
#
# cSmMat2 <- cacheSolve(mMat) # when cacheSolve of mMat is called again, the subfunction finds a cache and recalls it
# # getting cached matrix-inverse data
#
# cSmMat2
# # [,1]  [,2]
# # [1,] -1.0  0.75
# # [2,]  0.5 -0.25
#
# # try resetting mMat
# mMat$set(Mat*2)
#
# mMat$get()
# # [,1] [,2]
# # [1,]    4   12
# # [2,]    8   16
#
# mMat$getMatInv() # existing Matrix-Inverse is reset to NULL
# # NULL
#
# cSmMat3 <- cacheSolve(mMat)
# when cacheSolve of mMat is called after the resetting the matrix,
# the Matrix-Inverse which no longer exist now needs to be solved
# cSmMat3
# # [,1]   [,2]
# # [1,] -0.50  0.375
# # [2,]  0.25 -0.125
#
# mMat$getMatInv()
# # [,1]   [,2]
# # [1,] -0.50  0.375
# # [2,]  0.25 -0.125

# http://mathworld.wolfram.com/MatrixInverse.html
