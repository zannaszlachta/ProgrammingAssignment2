# A pair of functions to cache the inverse of a matrix
# (assumes that input is an invertible matrix - as per assignment instructions)


# makeCacheMatrix will create a list with four functions:
# 1. set: allows you to set/assign a matrix
# 2. get: retrieves the matrix
# 3. setInverse: you can set the inverse of the matrix
# 4. getInverse: retrieves the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #sets to NULL

  #set function allows you to set a matrix, e.g.
  #   x <- makeCacheMatrix()
  #   x$set(matrix(1:4,2,2))
  set <- function(y) {
    x <<- y
    inv <<- NULL  #sets inverse to NULL, as we have a new matrix
  }

  #get function will retrieve the set function, e.g.
  #   x$get() will print the matrix
  get <- function() x
  
  #setInverse allows to set the inverse (used later in cacheSolve)
  setInverse <- function(inverse) inv <<- inverse
  
  #getInverse prints the inverse
  getInverse <- function() inv
  
  #create a list with the four functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



# cacheSolve will solve the matrix, cache and print the inverse
# if it has already been solved, it will retrieve the cached value

cacheSolve <- function(x, ...) {

  # assigns the inverse to variable inv
  inv <- x$getInverse()
  
  # if inverse(inv) exists, it retrieves cached value and returns it
  # (and exits function)
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # if inverse(inv) doesn't exist, it assigns matrix to variable data,
  # then solves it and assigns inverse to variable inv,
  # then sets it with x$setInverse(inv), and finally prints it
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}