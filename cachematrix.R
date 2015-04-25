## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL ## set inverse to NULL to initialize it
  ## define a set function to set the value of x(cached value) which is valid for the function
  ##makeCacheMatrix
  set<-function(m){
    ## set the value of the x in this function makeCacheMatrix with the input parameter m
    x<<-m 
    ## Because the x changed in makeCacheMatrix, inv is no longer valid for the new x
    ## so invaliadte or initialize the inv
    inv<<-NULL
  }
  
  get<-function()x ##return the value of x(this is a cached value) which is valid for the function makeCacheMatrix
  ##cache the inverse of the matrix
  setinverse<-function(inverse)inv<<-inverse
  
  ## get the cached value of the inverse
  getinverse<-function()inv
  
  ##assign the fucntions to get & set the cached value of a matrix and its inverse to makeCacheMatrix
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse
    
    )
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  
  ##if inverse of a given matrix is cached
  if(!is.null(inv)){
    message("getting cached inverse")## give a message
    return(inv)## return the cached value of the matrix and stop further processing of this function
        
  }
  ## if the inverse of the matrix is not cached then calculate it and then cache it
  matrix<-x$get() ## get the cached value of the matrix to calculate its inverse
  inv<-solve(matrix)## calcualte the inverse
  x$setinverse(inv)## cache the calculated inverse of the matrix
  inv
  

}
