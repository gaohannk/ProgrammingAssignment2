## Put comments here that give an overall description of what your
## functions do
# The first function, makeCacheMatrix creates a special "matricx", which is really a list containing a function to

#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
   x_inverse <- NULL
   set <- function(y) {
           x <<- y
           x_inverse <<- NULL
   }
   get <- function() x
   set_inverse <- function(solve){
           x_inverse <<- solve
   }
   get_inverse <- function(){
           x_inverse
   }
   list(set = set, get = get, set_inverse = set_inverse, get_inverse= get_inverse)
}


## Write a short comment describing this function
#The following function calculates the inverse of the special "matrix" created with the above function. However, it first checks to see if the inverse has already been calculated. 
#If so, it gets the mean from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value  in the cache via the set_inverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x_inverse = x$get_inverse()
        if (!is.null(x_inverse))
                return(x_inverse)
        m  <- x$get()
        x_inverse = solve(m, ...)
        x$set_inverse(x_inverse)
        return(x_inverse)
}
