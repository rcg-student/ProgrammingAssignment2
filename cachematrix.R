makeCacheMatrix <- function(x= matrix()){
  # The goal of this function is to set, get the object x with one matrix 
  # and the cache m with the inverse of X.
  # We initialize the cache m with the value NULL
  m<-NULL
  #Then we can use four differnts function:
  set <- function(y) { x<<-y
  m<<-NULL}
  #SET to change the matrix in X for another one. We use '<<-' t set a value in x in the parent environment
  # and we have reseted the cache.
  get <- function() {x}
  #GET to see what is the value in x.
  setcache <- function(solve) {m<<- solve}
  #SETCACHE to set the cache with the inverse of x.
  getcache <- function() {m}
  #GETCACHE to see the value in the cache
  list( set = set, get = get,
        setcache = setcache,
        getcache = getcache)
  #Finally we assigns the values to a list wich is the result
}

cacheSolve <- function(x,...) {
  #The goal of this function is to give the inverse of the matrix
  m <- x$getcache()
  #To achieve that goal we take the value from the cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
    #If m is not null then we can take the result from the cache
  }
  data <- x$get()
  # If m is null, we take the matrix and we compute the inverse
  m <- solve(data,...)
  x$setcache(m)
  # Finally we save the result in the cache
  m
}

# One typical example of use will be:
# > a <- makeCacheMatrix(matrix(1:4,2,2))
# > a$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(a)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > a$set(matrix(4:7,2,2))
# > a$get()
# [,1] [,2]
# [1,]    4    6
# [2,]    5    7
# > cacheSolve(a)
# [,1] [,2]
# [1,] -3.5    3
# [2,]  2.5   -2

# If we don't use the functions in the correct order we're obteining some bugs
# This function returns an error when you ask for the inverse of the matrix and the result is not possible
# this is the message: I will catch it when I'll know how.
#  Error in solve.default(data, ...) : 
#    Lapack routine dgesv: system is exactly singular: U[3,3] = 0 