## This program will create a matrix and then allow for the 
## inversion of the matrix.  If the matrix has already been inverted 
## it pulls it from cache instead of redoing the work.  If not 
## inverted then it inverts the matrix and also saves a copy to a cache.


## Create a Matrix from passed matrix same size and shape.   

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    # Assign to x whatever the << operations does(skip scope?)
    x <<- y
    # Clear the inverted matrix holding variable. The matrix has been replaced.  
    m <<- NULL
  }
  get <- function() x
  # Save the inverted matrix. 
  setmatrix <- function(invertedMatrix) m <<- invertedMatrix
  # retreive the inverted matrix saved in m
  getmatrix <- function() m
  # return a list of the 4 functions created here.  Not sure how it gets
  # called in the first place. 
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Return the inverse of the passed matrix.  Cache inverse matrix allowing for
## checking if the matrix has already been inverted. if not inverted then 
## invert save and return. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # Retreive inverted matrix of x from the makeCacheMatrix object. 
  m <- x$getmatrix()
  # if already inverted return inverted matrix. 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # get the matrix from the makeCacheMatrix object using the get method. 
  data <- x$get()
  # inverse the matrix using the solve() function
  m <- solve(data, ...)
  # save the inverted matrix to the makeCacheMatrix object. 
  x$setmatrix(m)
  # return the inverted matrix
  m
}
