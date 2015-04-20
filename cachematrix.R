## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#Function to ob
makeCacheMatrix <- function(x = matrix()) {
                #set inverse matrix i as null
                i <- NULL
                #function set
                set <- function(y) {
                        #set value of matrix x
                        x <<- y
                        #set value of inverse matrix i
                        i <<- NULL
                }
                
                #get value of matrix x
                get <- function() x
                
                #set values of inverse matrix i
                setinverse <- function(inverse) i <<- inverse
        
                #get value of inverse
                getinverse <- function() i
                
                #set list of parameters
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
        }
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #obtain inverse from the matrix
        i <- x$getinverse()
        #if is not null obtains the value
        if(!is.null(i)) {
                message("getting cached data")
                #return matrix i and get out of the function
                return(i)
        }
        #else it obtains the inverse matrix
        data <- x$get()
        #calculates the inverse
        i <- solve(data)
        #set the mean for the vector
        x$setinverse(i)
        #return the inverse matrix i
        i
}
