
## makeCacheMatrix creates three functions createMatrix, storeMatrix and retrieveMatrix
## * createMatrix can be used to create a matrix (casheSolve will use it)
## * storeMatrix stores a matrix passed to it in cache
## * retrieveMatrix returns the matrix that is currently stored in cache

makeCacheMatrix <- function(x = matrix()) {
        ## mA is the cashe storage
        mA <- NULL
        ## * createMatrix generates a jxj matrix and fills it with random numbers and returns it
        createMatrix <-function(j){
                mA <<- matrix(rnorm(j^2),j,j)
                ## mA will hold the original not inverted matrix in the makeCasheMatrix environment
        }
        ## * storeMatrix stores a matrix passed to it in cache (makeCasheMatrix will pass the 
        ## inverted matrix of the matrix created in createMatrix)
        storeMatrix <-function(y){
                mA <<- y
                ## mA will hold the inverted matrix in the makeCasheMatrix environment
        }
        ## * retrieveMatrix returns the matrix that is currently stored in cache (the inverted one put there 
        ## storeMatrixby)        
        retrieveMatrix <-function(){
                mA
                ## "picking up" the inverted matrix from cache
        }
        ## makeCasheMatrix returns a list of functions callable by $name
        list (createMatrix=createMatrix,
              storeMatrix = storeMatrix,
              retrieveMatrix=retrieveMatrix)
}


## for every fith turn in the main loop in cacheSolve, it calls createMatrix from makeCacheMatrix
## to obtain a new matrix which it then inverses using the solve function 
## and then stores it in cache using storeMatrix
## for the 4 subsequent turns cacheSolve used retrieveMatrix to obtain the already inversed 
## matrix from cache
## this is done to demonstrate how cache can be used to avoid unnecessary repetion of computation
## after 25 turns in the loop it stops

cacheSolve <- function(x, ...) {
        i <- 25
        ## k will be used to control matrix size 2by; 3by3; 4by4; 5by5; 6by6
        k <- 1
        while (i>0) {
                ## A will be our original matrix and Ainv its inverse
                A<-NULL
                Ainv<-NULL
                print("new turn in the loop, matrices set to NULL")
                
                ## (i/5)%%1 returns the fraction of i/5; e.g if i is 22 it returns 0.4
                if((i/5)%%1==0){
                        ## this will be true for i equal to 25, 20, 15...
                        k = k+1
                        A <- x$createMatrix(k) 
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
                i = i-1
        }
}

## use this to test
## z<-makeCacheMatrix()
## cacheSolve(z)