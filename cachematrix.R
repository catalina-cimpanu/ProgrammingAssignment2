## Put comments here that give an overall description of what your functions do

## Write a short comment describing this function

makeCacheMatrix <- function(normal_matrix = matrix()) {
     # initialize inverse matrix
     inverse_matrix <- NULL
     
     # set the matrix
     set_matrix <- function(matrix) {
          normal_matrix <<- matrix 
          inverse_matrix <<- NULL
     }
     
     # get the matrix
     get_matrix <- function() {
          normal_matrix
     }
     
     # set inverse of the matrix
     set_inverse_matrix <- function(inverse) {
          inverse_matrix <<- inverse
     } 
     
     # get inverse of the matrix
     get_inverse_matrix <- function() {
          inverse_matrix
     }
     
     list(set_matrix = set_matrix, get_matrix = get_matrix,
          set_inverse_matrix = set_inverse_matrix,
          get_inverse_matrix = get_inverse_matrix)
}

# just copy paste of the code for the mean and changed the word "mean" with the word "matrix", 
# used as reference to create the function makeCacheMatrix
# makeCacheMatrix2 <- function(x = matrix()) {
#      m <- NULL
#      set <- function(y) {
#           x <<- y
#           m <<- NULL
#      }
#      get <- function() x
#      set_matrix <- function(matrix) m <<- matrix
#      get_matrix <- function() m
#      list(set = set, get = get,
#           set_matrix = set_matrix,
#           get_matrix = get_matrix)
# }


## Write a short comment describing this function

cacheSolve <- function(my_matrix, ...) {
     ## Return a matrix that is the inverse of 'my_matrix'
     
     # return a matrix that is the inverse of my_matrix
     my_inverse_matrix <- my_matrix$get_inverse_matrix()
     
     # just return the inverse if its already set
     if(!is.null(my_inverse_matrix)) {
          message("getting cached data")
          return(my_inverse_matrix)
     }
     
     # get the matrix from our object
     data <- my_matrix$get_matrix()
     
     # calculating the inverse
     my_inverse_matrix <- solve(data, ...)
     
     # set the inverse to the object
     my_matrix$set_inverse_matrix(my_inverse_matrix)
     
     # return the inversed matrix
     my_inverse_matrix
}

# just copy paste of the code for the mean and changed the word "mean" with the word "matrix", 
# used as reference to create the function cacheSolve
# cacheSolve2 <- function(x, ...) {
#      ## Return a matrix that is the inverse of 'x'
#      m <- x$get_matrix()
#      if(!is.null(m)) {
#           message("getting cached data")
#           return(m)
#      }
#      data <- x$get()
#      m <- solve(data, ...)
#      x$set_matrix(m)
#      m
# }
