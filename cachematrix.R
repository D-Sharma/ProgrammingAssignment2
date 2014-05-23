## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function creates a special matrix object and caches the inverse of the martix.
#It contains child functions which allows to set and get martix and cached inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
          #initialize matrix and cache symbol
            m <- NULL        #initialize cache symbol
            set <- function(y) {
              #sub function that sets new values of matrix object
              x <<- y        #set new values for matrix object in parent environment
              m <<- NULL     #initialize cache symbol in the parent environment
            }
            get <- function() x        #Sub function that returns current values of martix object
            setsolve <- function(solve) m <<- solve        
            getsolve <- function() m                        #Return matrix inverse from cache
            list(set = set, get = get,
                 setsolve = setsolve,
                 getsolve = getsolve)
}


## Write a short comment describing this function
#This function returns an inverse of the martix object from the cache. 
#The function checks whether a cache for the martix inverse exists. If a cache exists then it will return the object.
#However, if a cache of the martix inverse does not exist then it will create a matrix inverse, store it in cache, and return an inverse of the martix.
cacheSolve <- function(x, ...) {
            ## Return a matrix that is the inverse of 'x'
            m <- x$getsolve()      #Gets the existing matrix inverse 
  
            #Return the cached inverse matrix if it exists.
            if(!is.null(m)) {        #Verify if cached martix inverse exists
              message("getting cached data")
              return(m)          #Return cached martix inverse.
            }
            
            #Matrix inverse does not exist, hence create an inverse of the matrix and cache it
            data <- x$get()     #Get existing dataset 
            m <- solve(data)    #Create inverse of the matrix.
            x$setsolve(m)       #Store the martix inverse cache in the global environment which is the parent environment.
            m
}
