## R programming course: assignment 2
## Two functions: one that sets up a matrix object that can cache its inverse, 
##                another to calculate the inverse if it isn't present already.

## As specified in the question, I have assumed any input matrix is invertible by default.

########################################################################
## Creates a special "matrix" object [a list] that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) { # where no matrix argument specified, default to an empty matrix object
        m <- NULL
        set <- function(y) { 			#ingest matrix, reset cache
                x <<- y
                m <<- NULL
        }
        get <- function() x 			# return matrix
        setinverse <- function(inversecache) m <<- inversecache # stores the inverted matrix 
        getinverse <- function() m 		#...and then returns it
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) 		# last call returns this list of functions as our "matrix" object
}

##############################################################################
## Used in conjuntion with the above, will calculate/recall a matrix's inverse 

cacheSolve <- function(x, ...) { 			# feed in the "matrix" from above 
        m <- x$getinverse() 				# try to fetch the inverted matrix
        if(!is.null(m)) { 				# if the inverted matrix is cached, fetch it and be sure to let me know.
                message("getting cached data")
                return(m)
        }
        m <- solve(x$get(), ...)			# otherwise invert the matrix...
        x$setinverse(m)				# and cache it
        m
}
