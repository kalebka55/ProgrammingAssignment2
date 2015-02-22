makeCacheMatrix <- function(x=matrix(), ...) {  ## function to create an input square (invertible) matrix
                                                ## and store it in cache; also defines functions for later use in cacheSolve
                                                
        m <- NULL  ##  default for cacheSolve (i.e. if cacheSolve not initiated yet)
        
        setCacheMatrix <- function(y) {  ##  function to assign x (input matrix) & m into cache 
                x <<- y
                m <<- NULL
        }
        
        ##  functions of x to be stored for later use
        getCacheMatrix <- function() x  ##  function to return cached input matrix
        setInverseMatrix <- function(solve) m <<- solve  ##  function to store evaluated inverse matrix into cache as m
        getInverseMatrix <- function() m  ## function to return m (inverse matrix) if available
        list(setCacheMatrix=setCacheMatrix,  
             getCacheMatrix=getCacheMatrix,  
             setInverseMatrix=setInverseMatrix,
             getInverseMatrix=getInverseMatrix)  ## list to store above functions of x
}

cacheSolve <- function(x) {  ## function to search cache or evaluate inverse
        m <- x$getInverseMatrix()  ##  assign stored inverse matrix (if available) to m
        if(!is.null(m)) {  ## a conditional to check if inverse has been previously calculated - if conditional = TRUE, cached inverse printed to console 
                message("Getting cached inverted matrix...")
                message("Please be patient...")
                message("This may not take a while...")
        return(m) 
               
        }
        ##  if above conditional = FALSE, inverse is evaluated and stored into cache
        data <- x$getCacheMatrix()  ## calls for cached input matrix and assigns to 'data'
        m <- solve(data)  ## evaluates inverse of input matrix, and assigns to m
        x$setInverseMatrix(m) ## calls stored function to store newly evaluated inverse into cache as 'm'
        round(m, digits=3)  ## prints inverse to console
        
}
