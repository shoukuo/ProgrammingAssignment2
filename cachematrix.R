## The two functions are used to avoid calculating the inverse of a matrix repeatedly and hence
## save a lot of time.



## This function makeCacheMatrix is used to create a special "matrix", 
## which is really a list containing several functions to
## 1. set the value of the matrix   ---- set
## 2. get the value of the matrix   ---- get
## 3. set the value of the inverse  ---- setinverse
## 4. get the value of the inverse  ---- getinverse
## The value of the matrix and the inverse are always stored in the body of the function 
## makeCacheMatrix, the environment where the four functions mentioned above are defined.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function cacheSolve is used to calculate the inverse of the special "matrix" created
## with the above function, namely, the formal argument x is a list in fact. It first checks 
## to see if the inverse has already been calculated. If so, it gets the inverse and skips
## the computation. Othewise, it gets the value of the matrix and calculates the inverse and 
## sets the inverse to the cache. In the end, it returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return inv
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        
        inv
}
