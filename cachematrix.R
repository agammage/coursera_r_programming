# Author:
# Willemijn van Woerkom.

# Description:
# This script contains two functions that allow you to create a 'matrix' object
# that can store it's inverse, and a function that will either calculate the
# inverse, or retrieve it from memory to save unnecessary processing time.
# Below the function definitions, some example uses of the defined functions are
# included.

#### Functions ####

makeCacheMatrix <- function(x = matrix()) {
        # This function returns a list that contains four custom functions that
        # can be used to construct a special "matrix" object that can cache its
        # inverse. In case no matrix is entered, a default empty matrix is
        # created.
        
        # Set the inverse of the matrix to NULL since it hasn't been calculated
        # yet at this point.
        i <- NULL
        
        # Define two functions that:
        # 1. set the matrix object in the environment, and
        # 1. get the matrix object from the environment.
        set <- function(y) {
                # This function saves the entered matrix in cache, and resets
                # the inverse to NULL (in case it had been stored for a
                # previously entered matrix).
                x <<- y
                i <<- NULL
        }
        get <- function() x
        
        # Define two functions that:
        # 1. set the inverse of the matrix in the environment,
        # 2. get the inverse of the matrix from the environment.
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        
        # Return the list with the functions defined above.
        list(
                set = set,
                get = get,
                setinv = setinv,
                getinv = getinv
        )
}

cacheSolve <- function(x, ...) {
        # This function computes the inverse of the special "matrix" returned by
        # makeCacheMatrix above. It takes the list created by makeCacheMatrix as
        # input (x).
        # If the inverse has already been calculated (and the matrix has not
        # changed), then cacheSolve retrieves the inverse from the cache.
        # Note: this function assumes that the entered matrix is invertible.
        
        # Attempt to load the cached inverse from the matrix object.
        inv <- x$getinv()
        
        # If the load is successful (i.e. if the loaded object is not NULL),
        # return the cached inverse and exit the function, skipping the
        # following computation.
        if(!is.null(inv)) {
                message("Getting cached inverse.")
                return(inv)
        }
        
        # If the inverse has not been calculated yet: calculate it, store it in
        # the matrix object's cache, and return the inverse.
        message("Calculating the inverse.")
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

#### Example use ####

# Define an invertible matrix to use.
invertible.matrix <-
        matrix(c(-3, -2, -3, 5, 3, 5, 2, 1, 1), nrow = 3, byrow = T)

# Create the list with functions, and store the invertible matrix in memory.
my.mat.fun <- makeCacheMatrix(invertible.matrix)
# Alternatively, you can use the set() function from the list you just created
# to store the invertible matrix in memory.
my.mat.fun$set(invertible.matrix)

# Get the inverse of the matrix, either by retrieving it from memory, or by
# calculating it.
# Note: this line will produce a difference output the second time you run it
# than the first time you run it, since the first time you run it will result in
# the inverse being stored in memory.
cacheSolve(my.mat.fun)
