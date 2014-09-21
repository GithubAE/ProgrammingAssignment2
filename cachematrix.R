



makeMatrix <- function(mat) {
        
   
        solved <- NULL                          #sets the inverse matrix to zero
       
        
        
        set <- function(y) {                    #creates a new matrix with new input dta x
                mat <<- y
                solved <- NULL                  #sets the inverse matrix to zero
                
        }
        
        get <- function() mat                    #gets the matrix 
        setinverse <- function(solve) solved <<- solve
        getinverse <- function() solved
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



cacheMatrix <- function(x, ...) {
        solved <- x$getinverse()
        if(!is.null(solved)) {
                message("getting cached data")
                return(solved)
        }
        data <- x$get()
        solved <- solve(data, ...)
        x$setinverse(solved)
        solved
}