
makeCacheMatrix<-function(x){ #x is an invertible matrix
  
  
  m <- matrix(1:2,2,1) #holds inverse matrix, or 1X2 matrix otherwise
  
    
  set <- function(y){ #create new matrix, and reset m
    x <<- y
    m <<- matrix(1:2,2,1)
    message("code running in set")
    
  }   
  get <- function() x #returns matrix x  
  
  
  setInverse <- function(){ #gets inverse of x, and sets m to result
    m <<- solve(x)   
  } 
  
  getInverse <- function() m #returns matrix m
  
  #List allows for use of the functions after assignment
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse) 
  
} 

cacheSolve<-function(x,...){#Given a makeCacheMatrix object, will return
                            #inverse of the matrix
  
  m <- x$getInverse() #Pulling inverse matrix (if known)
                          #or 2x1 default matrix
  
  if(nrow(m)==ncol(m)){ #i.e. inverse matrix is already known
    message("Getting cached data...")
    return(m)
  }
  
  
  message("Generating results and caching...")
  x$setInverse() #solve for invertible matrix, and assign to m
  m<-x$getInverse()
  return(m) #returns m 
  
}