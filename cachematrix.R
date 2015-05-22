
## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to

# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function( m = matrix() ) {
      i <- NULL
# 1. Set the value of matrix
      set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
      }

# 2. Get the value of the matrix      
      get <- function() m

# 3. Set the value of the inverse
      setInverse <- function(inverse) i <<- inverse

# 4. Get the value of the inverse
      getInverse <- function() i
      list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}



##The following function calculates the inverse of the special "matrix" created with the above function.
##However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from 
##the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value 
##of the inverse in the cache via the setinverse function.

# 1. set a matrix as the inverse of the special matrix 'x'
# 2. check if inverse is already calculated. If so, skip computation
# 3. set special "matrix"
# 4. calculate inverse
# 5. set inverse
# 6. return inverse

cacheSolve <- function(x, ...) {
# 1. set a matrix as the inverse of the special matrix 'x'
      i <- x$getinverse()
# 2. Check if inverse is already calculated. If so, skip computation
      if(!is.null(i)) {
              message("getting cached data")
              return(i)
      }
# 3. set special "matrix"
      data <- x$get()
# 4. calculate inverse
      i <- solve(data, ...)
# 5. set inverse
      x$setinverse(i)
# 6. return inverse
      i
}
