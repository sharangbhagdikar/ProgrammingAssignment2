## Basically, I am trying to cache the inverse of a matrix to speed up processing
## time. As matrices get larger and more complicated, matrix inversion by Gaussian
## Elimination becomes extremely time consuming. Thus to avoid constantly having
## to compute the inverse we instead get it cached, in order to retrieve at leisure

## Takes the matrix 'x' as argument and creates a special matrix. returns a list of functions. Similar to the one illustrated in the sample example 

makeCacheMatrix <- function(x = matrix()) {
        im<-NULL
        set<-function(y){
                x<<-y
                im<<-NULL
        }
        get<-function() x
        setinverse<-function(inverse) im<<-inverse
        getinverse<-function() im
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


##Takes the special matrix as input and returns the inverse of matrix 'x' by computing if not previously computed, or by invoking its cache value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im<-x$getinverse()
        if(!is.null(im)){
                message("Returning Cached Value")
                return (im)
        }
        data<-x$get()
        im<-solve(data)
        x$setinverse(im)
        im
}
