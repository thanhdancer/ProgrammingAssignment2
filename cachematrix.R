#' Create a special "matrix" object that can cache its inverse
#'
#' This function creates a special "matrix" object that can cache its inverse.
#' This is useful for optimizing computations by storing the result of an expensive operation
#' and reusing it when needed, rather than recomputing it.
#'
#' @param x A matrix (default is an empty matrix).
#' @return A list containing four functions:
#' \describe{
#'   \item{set}{Assigns a new matrix to the special "matrix" object and resets the cached inverse.}
#'   \item{get}{Retrieves the current matrix.}
#'   \item{setInverse}{Caches the inverse of the matrix.}
#'   \item{getInverse}{Retrieves the cached inverse of the matrix.}
#' }
#' @examples
#' mat <- matrix(c(1, 2, 3, 4), 2, 2)
#' cacheMat <- makeCacheMatrix(mat)
#' cacheMat$get()
makeCacheMatrix <- function(x = matrix()) {
    invertedMatrix <- NULL
    set <- function(y) {
        x <<- y
        invertedMatrix <<- NULL
    }

    get <- function() x

    setInverse <- function(inverse) invertedMatrix <<- inverse
    getInverse <- function() invertedMatrix

    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

#' Compute the inverse of a matrix and cache the result
#'
#' This function computes the inverse of a matrix. If the inverse has already been calculated 
#' (and the matrix has not changed), it retrieves the inverse from the cache to avoid redundant computations.
#'
#' @param x A special "matrix" object created by the `makeCacheMatrix` function, which contains functions to 
#' set and get the matrix and its inverse.
#' @param ... Additional arguments to be passed to the `solve` function.
#' @return A matrix that is the inverse of the input matrix 'x'. If the inverse has already been calculated and 
#' cached, it returns the cached inverse matrix.
#' @examples
#' mat <- matrix(c(1, 2, 3, 4), 2, 2)
#' cacheMat <- makeCacheMatrix(mat)
#' cacheSolve(cacheMat)
cacheSolve <- function(x, ...) {
    invertedMatrix <- x$getInverse()
    if(!is.null(invertedMatrix)) {
        message("getting cached data")
        return(invertedMatrix)
    }

    data <- x$get()
    invertedMatrix <- solve(data, ...)
    x$setInverse(invertedMatrix)
    invertedMatrix
    ## Return a matrix that is the inverse of 'x'
}
