## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix allows a matrix or its inverse to be stored and retrieved
# cacheSolve returns a cached inverse or solves for the inverse
# depending on whether an inverse is already cached or not


## Write a short comment describing this function
## makeCacheMatrix takes a matrix input and stores a series of functions that
## allow the original matrix or a stored inverse of the matrix to be set or accessed
makeCacheMatrix <- function(x = matrix()) {
        #create variable to contain inverse matrix
        solved <- NULL
        
        #set function assigns new matrix and clears cache
        set <- function(y) {
                x <<- y
                solved <<- NULL
        }
        #get returns original matrix
        get <- function() x
        
        #setsolved assigns inverse matrix to solved variable
        setsolved <- function(s) solved <<- s
        
        #getsolved returns inverse matrix stored in solved variable
        getsolved <- function() solved
        
        #list of functions
        list(set = set, get = get,
             setsolved=setsolved,
             getsolved = getsolved)  
        
}


## Write a short comment describing this function
## cacheSolve checks to see if there is a cached value of the matrix inverse
## stored in the object we created with makeCacheMatrix - if there is, it returns it
## if there isn't it solves for the inverse and stores that in the cache variable
cacheSolve <- function(x = matrix()) {
        ## Return a matrix that is the inverse of 'x'
        
        #call getsolved function, if not Null (i.e. there is cached inverse)
        #display message and return cached inverse
        s <- x$getsolved()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        #otherwise get matrix, storing as 'data', solve, and call
        #set solve to save inverse to the cache 'solved' variable
        #then return inverse
        data <- x$get()
        s <- solve(data)
        x$setsolved(s)
        s
}