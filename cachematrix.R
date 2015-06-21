## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
		inver <- NULL
		set <- function(mat) 
		{
			x <<- mat
			inver <<- NULL
		}
		get <- function() x
		setinverse <- function(inverse) inver <<- inverse
		getinverse <- function() inver
		list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Below function Calculate Inverse of a matrix. It first check that inverse of a matrix
## is already calculated or not. If calculated then it returns the result from cache using
## setinverse() function.
  
## Function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
        inver <- x$getinverse()
			if(!is.null(inver)) 
			{
				message("getting cached data.")
				return(inver)
			}
			data <- x$get()
			inver <- solve(data)
			 x$setinverse(inver)
			  inver
}


## Sample Test And My Results :
## > x = rbind(c(4,3),c(3,2))
## > m = makeCacheMatrix(x)
## > m$get()
##	     [,1] [,2]
## [1,]    4    3
## [2,]    3    2
##
##
## > cacheSolve(m)
##       [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4
##
##
## > cacheSolve(m)
## getting cached data.
##       [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4


