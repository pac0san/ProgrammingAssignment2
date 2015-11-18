################################################################################
# Matrix Inverse Calculation with Cache                          Version 1.1.0 #
# Copyright 2015 FSB                                 All Rights Reserved.      #
# E-Mail: r.programming@icloud.com                   License: AGPLv3           #
# Created  16/11/2015                                Last Modified: 18/11/2015 #
################################################################################
# Use it (in R):                                                               #
# > M <- makeCacheMatrix(Square_and_not_Singular_Matrix)                       #
# > M_Inverse <- cacheSolve(M)                                                 #
################################################################################
########################### License & Copyright Header #########################
#                                                                              #
#                 Copyright (c) 2015 r.programming@icloud.com                  #
#                                                                              #
# This program is free software: you can redistribute it and/or modify it      #
# under the terms of the GNU Affero General Public License as published by the #
# Free Software Foundation, either version 3 of the License, or (at your       #
# option) any later version.                                                   #
#                                                                              #
# This program is distributed in the hope that it will be useful, but WITHOUT  #
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or        #
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License #
# for more details.                                                            #
#                                                                              #
# You should have received a copy of the GNU Affero General Public License     #
# along with this program.  If not, see <http://www.gnu.org/licenses/>.        #                                                                    #
#                                                                              #
######################## End License & Copyright Header ########################

##
# Creating and using an auxiliary matrix, just for testing purposes:
# - Create a square 10x10 matrix of integers (-100:100)
#   > matriz <- matrix(sample(-100:100, 100, replace = TRUE) ,10 ,10)
# - Must be not-sigular (if not, try creating another one):
#   > det(matriz) != 0
# - Write data to file:
#   > write.table(matriz, file = "matrixdata.dat", col.names = F, row.names = F)
# - Read matrix data:
#   > matriz <- matrix(scan("matrixdata.dat", n = 10*10), 10, 10, byrow = TRUE)


##
# makeCacheMatrix: creates a special "Matrix", which is really a list containing
#                  a function to
#
# - set: set the value of the Matrix
# - get: get the value of the Matrix
# - setInverse: set the value of the Matrix's Inverse
# - getInverse: get the value of the Matrix's Inverse
#

makeCacheMatrix <- function(M = matrix()) {
  Inverse <- NULL
  set <- function(N) {
    M <<- N
    Inverse <<- NULL
  }
  get <- function()
    M
  setInverse <- function(Inv)
    Inverse <<- Inv
  getInverse <- function()
    Inverse
  list(
    set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


##
# cacheSolve: computes the inverse of the special "matrix" returned by
#             'makeCacheMatrix' above. If the inverse has already been
#             calculated (and the matrix has not changed), then the
#             'cacheSolve' should retrieve the inverse from the cache. 
#

cacheSolve <- function(M, ...) {
  ## Return a matrix that is the inverse of 'M'
  Inverse <- M$getInverse()
  if (!is.null(Inverse)) {
    message("getting cached data")
    return(Inverse)
  }
  data <- M$get()
  Inverse <- solve(data, ...)
  M$setInverse(Inverse)
  Inverse
}
