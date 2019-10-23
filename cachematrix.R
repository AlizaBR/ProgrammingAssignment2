#This two functions work togather. The first one creates an object of class list
#with a matrix and it's inverse. The second one uses as argument the object 
#returned by the first one, retrieving it's elements: it's actually what makes
#it run. It calculates the inverse of a matrix and stores it in the cache;
#before doing so, it looks for the value in the cache memory and if the inverse
#of the matrix is already stored, it get's it from there; if it's a "new"
#matrix and the value is not stored, it calculates the inverse and stores it.

# makeCacheMatrix: creates an R object that stores a matrix and it's inverse.
makeCacheMatrix <- function(x = matrix()) { 
        inv <- NULL 
        set <- function(y) { #Set the value of the matrix
                x <<- y
                inv <<- NULL
        }
        get <- function() x #Get the value of thematrix
        setinv <- function(solve) inv <<- solve #Set the value of the inverse matrix
        getinv <- function() inv #Get the value of the inverse matrix
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}


#cacheSolve: it's argument is the object returned by makeCacheMatrix; it
#retrieves the inverse matrix from the cached value that is stored in the
#makeCacheMatrix() object's environmnent. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        message("calculating inverse")
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}