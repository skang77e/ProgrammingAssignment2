## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix creates a special "matrix", which is
## a set of functions to 

## 1. set the value of the matrix 
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

## then returns the those functions within a list 
## to the parent environment

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL   ##Initialize objects
  set <- function(y){ ## Define set() function
    x <<- y           ## Assign the input argument to the x object in the parent environment
    inv <<- NULL      ## Assign the value of NULL to the inv object in the parent environment. This line of code clears any value of inv that had been cached by a prior execution of cacheSolve().
  }
  get <- function() x ## Defines the getter for the vector x.
  setInv <- function(solve) inv <<- solve ## Defines the setter for the solve (inverse) inv.
  getInv <- function() inv ## Defines the getter for the solve(inverse) inv
  list(      ## Create a new object by returning a list()
       set = set, ## gives the name 'set' to the set() function defined above
       get = get, ## gives the name 'get' to the get() function defined above
       setInv = setInv, ## gives the name 'setInv' to the setInv() function defined above
       getInv = getInv  ## gives the name 'getInv' to the getInv() function defined above

      )
}

## Write a short comment describing this function
## cacheSolve calculates the inverse matrix of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse matrix has already been calculated.
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse matrix of the data and sets the value of the inverse matrix in the cache via the setInv function.

cacheSolve <- function(x, ...){
  ## Return a matrix that is the inverse of 'x'

  inv <- x$getInv() ## Attempts to retrieve a inverse from the object passed in as the argument
  if(!is.null(inv)){ ## checks to see whether the result is NULL
    message("getting cached data") ## we have a valid, cached matrix and can return it to the parent environment
    return(inv)
  }
  data <- x$get()         ## If the result of !is.null(m) is FALSE, cacheSolve() gets the matrix from the input object, 
  inv <- solve(data, ...) ## calculates a solve(), 
  x$setInv(inv)           ## uses the setInv() function on the input object to set the solve in the input object,
  inv                     ## and then returns the value of the solve to the parent environment by printing the solve object
}
