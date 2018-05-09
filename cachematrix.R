#############################################################################################
## Coursera - R Programming by Johns Hopkins University                                    ##
##                                                                                         ##
## Programming Assignment 2                                                                ##
## ------------------------                                                                ##
## Pair of R function to cache the potentially time-consuming matrix inversion values.     ##
##                                                                                         ##
#############################################################################################

## makeCacheMatrix
## Main function. Stores a matrix inside an object that has the setters and getters for the matrix and its inversion.
## Usage example: 
## mxTest <- matrix(c(4,2,7,6),ncol=2,nrow=2) #create a matrix
## j<- makeCacheMatrix(mxTest) #store this matrix in j
## j$get() #retrieves the matrix
makeCacheMatrix <- function(x = matrix()) {
        mxInverse <- NULL #initialize the variable for the inverse of the matrix
        
        set <- function(y){
                x <<-y #stores the matrix
                mxInverse <<- NULL #initialize the variable for the inverse of the matrix
        }
        
        get <- function() x
        
        setInverse <- function(inverse){
                mxInverse <<- inverse #stores the inverse of the matrix
        }
        
        getInverse <- function() mxInverse
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) #expose the inner functions externally.
}


## cacheSolve
## Gets a makeCacheMatrix objetc, recover the value of the inverse of the inner matrix cache and if it is not there, calculate it and store it in the cache.
## Usage example:
## k<-cacheSolve(j) #stores the inverse of the matrix j (created using makeCacheMatrix) in k.
## if you want to check if the inverse is correct: matrix * inverse = Identity
## iden<-apply((j$get() %*% k),2,FUN=round)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mxInverse <- x$getInverse()
        if(!is.null(mxInverse)){
                message("getting cached data")
                return(mxInverse)
        }
        data <- x$get()
        mxInverse <- solve(data,...) #uses the solve R function to calculate the inverse of the matrix. 
        x$setInverse(mxInverse)
        mxInverse
}
