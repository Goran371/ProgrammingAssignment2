## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates three function createMatrix, storeMatrix and retrieveMatrix
## createMatrix generates a matrix and fills it with random numbers and returns it
## storeMatrix stores a matrix in cache
## retrieveMatrix returns the matrix that is currently stored in cache

makeCacheMatrix <- function(x = matrix()) {
        mA <- NULL
        createMatrix <-function(){
                mA <<- matrix(rnorm(25),5,5)
        }
        storeMatrix <-function(y){
                mA <<- y
        }
        retrieveMatrix <-function(){
                mA
        }
        list (createMatrix=createMatrix,
              storeMatrix = storeMatrix,
              retrieveMatrix=retrieveMatrix)
}


## for every fith turn in the main loop in cacheSolve uses createMatrix from makeCacheMatrix to
## obtain a new matrix, inverses ## it and stored the inversed matrix in cache using storeMatrix
## for the 4 subsequent turns cacheSolve used retrieveMatrix to obtain the already inversed matrix from cache
## after 25 turns in the loop it stops

cacheSolve <- function(x, ...) {
        i <- 25
        while (i>0) {
                A<-NULL
                Ainv<-NULL
                print("new turn in the loop, matrices set to NULL")
                print(A)
                print(Ainv)
                if((i/5)%%1==0){
                        A <- x$createMatrix() 
                        print("new matrix retrieved from cache")
                        print(A)
                        Ainv<-solve(A)
                        x$storeMatrix(Ainv)
                        print("inversed matrix stored in cache")
                        print(Ainv)
                } else {
                        Ainv<-x$retrieveMatrix()
                        print("inversed matrix retrieved from cache")
                        print(Ainv)
                }
                i =i-1
        }
}
