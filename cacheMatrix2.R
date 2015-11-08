# programming in R assignment 2

# makeVector caches a numeric vector with set and a mean() with setmean().
# both are remembered in makeVector (as long as you are in makeVector)
# it does not calculate the mean (that is the responsibility of the caller)

makeVector <- function(x = numeric()) {
        m <- NULL
# 4 functions beind defined in makeVector
        set <- function(y) {
                # x in makeVector env gets the value of y
                x <<- y
                m <<- NULL
        }
        get <- function() {
                x
        } 
        setmean <- function(mean) {
                # m in makeVector env gets the value of mean
                m <<- mean
        }
        getmean <- function() {
                m
        }
        listig <- list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
        listig
}

# use see below
cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # note that there is no need to set() the data as it is passed into 
        # makeVector env by the call
        data <- x$get()
        # we have to calculate the mean as makeVector does not do that
        m <- mean(data, ...)
        # and make makeVector "remember" it
        x$setmean(m)
        m
}


# Intended usage
# use makeVector(numeric vector) and save in an object
# e.g. m1<-makeVector(20:30)
# call cachemean(m1) repeatedly
# first ime if will calculate the mean then it will take it from cache

# Assignment: Caching the Inverse of a Matrix
# Matrix inversion is usually a costly computation and their may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly
# (there are also alternatives to matrix inversion that we will not discuss here). 
# Your assignment is to write a pair of functions that cache the inverse of a 
# matrix.
# Write the following functions:
        
# 1.	makeCacheMatrix: This function creates a special "matrix" object that 
# can cache its inverse.
# 2.	cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the 
# inverse from the cache.

makeCacheMatrix <- function(x,...) {
        m <- NULL
        # 4 functions being defined in makeCacheMatrix
        # store the matrix with set
        set <- function(y) {
                # x in makeCacheMatrix env gets the value of y
                x <<- y
                m <<- NULL
        }
        # retrieve the matrix
        get <- function() {
                x
        } 
        # store the inversed matrix
        setmatrix <- function(matr) {
                # m in makeCacheMatrix env gets the value of matr
                m <<- matr
        }
        # retrieve the cached mateix
        getmatrix <- function() {
                m
        }
        listig <- list(set = set, get = get,
                       setmatrix = setmatrix,
                       getmatrix = getmatrix)
        listig
}

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # note that there is no need to set() the data as it is passed into 
        # makeCacheMatix env by the call
        data <- x$get()
        # we have to invert the matrix as makeCacheMatrix does not do that
        m <- solve(data, ...)
        # and make makeCacheMatrix "remember" it
        x$setmatrix(m)
        m
}

m2 <- matrix(data=1:9, nrow=3, ncol=3)
funclist <- makeCacheMatrix(m2)
cacheSolve(funclist)
cacheSolve(funclist)
