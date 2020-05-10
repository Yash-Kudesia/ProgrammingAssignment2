## load the matlib library using library(matlib)
## this library has a function inverse for calculating the inverse of matrix

## makeCacheMatrix take x as a mtrix and initializes it's environment

makeCacheMatrix <- function((x = matrix()) {
        m <- matrix()
        set <- function(y) {
                x <<- y
                m <<- matrix()
        }
        get <- function() x
        setinverse <- function(inv) m <<- inv
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function calculate the inverse of a matrix using the inv function of matlib library

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         m <- x$getinverse()
         #checking if the inverse already calculated for the matrix
        if(!is.null(m)) {
                #this temp has the  original matrix X
                temp<-x$get()
                #but before calling cacheSolve, matrix x may be modified, so to check whether x and temp are same we use the beloew if condition
                #so temp is before modification and x is after modification  (if any modification done)
                if(is.matrix(x) && is.matrix(temp) && dim(x) == dim(temp) && all(x == temp)==true){
                        #this if condition ensures that x is not modified
                        message("getting cached data")
                        return(m)
                }
        }

        m<- inv(x)
        x$setinverse(m)
        m
}
