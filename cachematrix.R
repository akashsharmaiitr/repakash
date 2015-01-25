## The following two functions are used to cache inverse of a matrix
##(matrix is invertible).Caching reduces the computation and hence it
##is time consuming

##The first function, makeMatrix creates a special "Matrix", which is really a list containing a function to
##1.Get the value of the Matrix
##2.Get the value of the Matrix
##3.Set the value of the Inverse Matrix
##4.Get the value of the Inverse Matrix

makeCacheMatrix = function(x = matrix()) {
    iv = NULL
    set <- function(y) {
        x <<- y
        iv <<- NULL
    }
    get = function() 
    x  
    setinverse = function(inverse)
    iv <<- inverse  
    getinverse = function()
    iv  
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

##The following function returns the inverse of the special matrix created in the first function.
##It first checks if the inverse has already been computed.If so,it skips the computational 
##part and gets the result.If not,it computes the inverse of the matrix 
##and sets the value in the cache function via setInverse function.

cacheSolve = function(x, ...) {
    iv = x$getinverse()
    if(!is.null(iv)) {
        message("getting cached data.")
        return(iv)
    }
    mat = x$get()
    iv = solve(mat)
    x$setinverse(iv)
    iv
}

##Running the program
##Sample1
##> x=matrix(c(2,3,4,5),2,2)
##> m=makeCacheMatrix(x)

##getting the matrix

##> m$get()
##     [,1] [,2]
##[1,]    2    4
##[2,]    3    5

##In first run there is no cache.
##> cacheSolve(m)
##       [,1] [,2]
##[1,] -2.5    2
##[2,]  1.5   -1


##Second run produces cache and retrieve the data.
##> cacheSolve(m)
##getting cached data.
##       [,1] [,2]
##[1,] -2.5    2
##[2,]  1.5   -1

##Sample 2
##> x=matrix(c(2,1,0,0,2,2,1,0,4),3,3)
##> m=makeCacheMatrix(x)

##getting the matrix
##> m$get()
##     [,1] [,2] [,3]
##[1,]    2    0    1
##[2,]    1    2    0
##[3,]    0    2    4
 
##In first run there is no cache
##> cacheSolve(m)
##           [,1]              [,2]              [,3]
##[1,]  0.4444444  0.1111111 -0.11111111
##[2,] -0.2222222  0.4444444  0.05555556
##[3,]  0.1111111 -0.2222222  0.22222222

##In second run,Retrieving cache
##> cacheSolve(m)
##getting cached data.
##           [,1]              [,2]              [,3]
##[1,]  0.4444444  0.1111111 -0.11111111
##[2,] -0.2222222  0.4444444  0.05555556
##[3,]  0.1111111 -0.2222222  0.22222222
