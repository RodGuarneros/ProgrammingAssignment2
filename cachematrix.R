## Put comments here that give an overall description of what your
## functions do 

## Having in mid that Matrix inversion is the process 
## of finding the matrix B that satisfies 
## the equation AB=I for a given invertible matrix A.
## Is is almost a substitution in both equations so as to:
## (i) get the special matrix instead of a vector and, and (ii) 
## get the inverse cache matrix.

## Write a short comment describing this function
## This function creates and gets a special "matrix" that will be
## send to the next function, as well as solve the matrix
## setting and getting the solution in cache:

#Creating a special matrix so as to cache the inverse

#Creating a special matrix so as to cache the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinversemtx <- function(solve) m <<- solve
        getinversemtx <- function() m
        list(set = set, get = get,
             setinversemtx = setinversemtx,
             getinversemtx = getinversemtx)
}


## Write a short comment describing this function

#Getting the inverse cached
#This function review if the inverse has been calculated
#if is is true (v.gr. !is.null(m))) avoid the operations
#made previously and get the previous result sotored in cache.

cacheSolve <- function(x, ...) {
        m <- x$getinversemtx ()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinversemtx(m)
        m
}

