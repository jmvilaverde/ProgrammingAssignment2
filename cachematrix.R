## Functions:
## makeCacheMatrix to manage a matrix and his inverse matrix

## cacheSolve to resolve the inverse of the matrix. 
## if inverse matrix for this matrix has been previously obtained, it is recovered, else is calculated and cached.


#Function to store and manage a Matrix and his Inverse Matrix
#implemented get and set functions for the Matrix
#implemented getinverted and setinverted functions for the Inverse Matrix
makeCacheMatrix <- function(x = matrix()) {
                #set inverse matrix i as null
                i <- NULL
                
                #function set for the Matrix x
                set <- function(y) {
                        #set value of matrix x
                        x <<- y
                        #set value of inverse matrix i to null, because the Matrix x has changed
                        i <<- NULL
                }
                
                #get value of Matrix x
                get <- function() x
                
                #set values of Inverse Matrix i
                setinverse <- function(inverse) i <<- inverse
        
                #get value of Inverse Matrix i
                getinverse <- function() i
                
                #set list of parameters
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
        }
}

#Function to obtain the Inverse Matrix of Matrix x passed as parameter
#if the inverse matrix exists for matrix x, it only gets the cached value, else the inverse matrix is calculated and cached.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #obtain inverse from the matrix
        i <- x$getinverse()
        
        #if is not null get the cached value
        if(!is.null(i)) {
                message("getting cached data")
                #return inverse matrix i and return from the function
                return(i)
        }
        #else it obtains the inverse matrix
        data <- x$get()
        #calculates the inverse
        i <- solve(data)
        #set the inverse for the matrix
        x$setinverse(i)
        #return the inverse matrix i
        i
}
