## This function takes a matrix as input and 
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
## It is to be noted that you can not anonymously define t he matrix in this function. 
## It must be deined explicitely and fed as an argument into the function

makeCacheMatrix <- function(x = matrix())
{

    m <- NULL
    set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## The following function calculates the mean of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and
## sets the value of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...)
{
  ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}