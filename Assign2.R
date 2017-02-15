#the makecachematrix function:
makeCacheMatrix <- function(x = matrix()) {
      
      #prepare the set function
      #initialize the value
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      
      #get function:
      get <- function() x
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      list(set = set,
           get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


#the cacheSolve function:
cacheSolve <- function(x, ...) {
      
      #inverse from the cache
      inv <- x$getInverse()
      if (!is.null(inv)) {
            #sent the message info
            message("getting cached data")
            return(inv)
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      #inverse 
      x$setInverse(inv)
      inv
}

