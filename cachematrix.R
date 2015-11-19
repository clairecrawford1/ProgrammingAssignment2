
## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    # create local m and set it to NULL
    m <- NULL
    
    # create 'set' function that will store the values of x and m
    # taking argument 'y' and setting that as the x value, and 
    # initializing m to NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # create function 'get' that will return the value of x
    get <- function() x
    
    # create setinverse function that will set the value of m
    # to the 'inverse' argument passed to it
    setinverse <- function(inverse) m <<- inverse
    
    # create getinverse function that will return the value of m
    getinverse <- function() m
    
    # return a list where each of the new functions is saved  
    # to their name
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## computes the inverse of the special "matrix" returned by 
## makecachematrix above.  If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve will retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # First, save the $getinverse value for this list to m so we can 
    # check it. 
    m <- x$getinverse()
    
    # if m is not null, we have already calculated the inverse for this matrix
    if(!is.null(m)) {
        # tell the user we are using cached data
        message("getting cached data")
        # and print the value of m
        return(m)
    }
    
    # if m is null, we will need to calculate the inverse
    # pull the matrix from the list using $get and store it as 'data' 
    data <- x$get()
    
    # use the 'solve' function to calculate the inverse of x
    # store the value as m
    m <- solve(data, ...)
    
    # store the newly calculated inverse to 'm' using the 'setinverse'
    # function
    x$setinverse(m)
    
    # return the value of 'm' to the user
    m
}
