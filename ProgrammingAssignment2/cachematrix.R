##Pair of functions used to create a special matrix object that can cache's its inverse.

# PART 1: Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {             
  
  #@x: a square invertible matrix
  
  ## Initialize the inverse property
  inv <- NULL
  
  ## Method to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Method to get the matrix
  get <- function() x
  
  ## Method to set the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse   #calculate the inverse
  #Can also be written as... function() inv <<- solve(x)
  
  ## Method to get the inverse of the matrix
  getinverse <- function() inv
  
  ## Return a list of the methods (or a list containing functions) to
  ##      1. set the matrix
  ##      2. get the matrix
  ##      3. set the inverse
  ##      4. get the inverse
  ## this list is used as the input to cacheSolve()
 
   list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## PART 2:
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## @x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  ## Just return the inverse if its already been calculated.
  
  #if the inverse has already been calculated
  if( !is.null(inv) ) {
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  ## otherwise, calculates the inverse 
  
  # Get the matrix from our object
  mat.data <- x$get()
  
  # Calculate the inverse using matrix multiplication
  inv <- solve(mat.data, ...) 
  
  ## Set the inverse to the object in the cache via the setinverse function.
  x$setinverse(inv)
  
  ## Return the matrix
  inv
}

# Testing
# M <- matrix(c(1,2,3,4),2,2)
# M1 <- makeCacheMatrix(M)
# cacheSolve(M1) 
###      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5


#To test out these functions. let us write a function called test(), 
#which takes in any invertible matrix, calculates its inverse twice using the above functions, 
#and prints out the times it takes for both runs. The first run should take longer than the 
#second run because first run actually calculates the inverse while the second run only does a look-up 
#from the cache.

test = function(mat){
  ## @mat: an invertible matrix
  
  temp = makeCacheMatrix(mat)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
}


#Let's try it on a matrix of 1000 rows and 1000 columns filled with normal random numbers.

set.seed(1110201)
r = rnorm(1000000)
mat1 = matrix(r, nrow=1000, ncol=1000)
test(mat1)

#**Output**
# Time difference of 1.398275 secs
# getting cached data
# Time difference of 0.007997036 secs

#The time diff from the first run is 1.34 seconds, and from the second run is 0.00799 seconds. 
#There is 99.4% decrease in time. This is the power of caching!