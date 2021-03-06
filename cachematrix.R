# Este script almacena en cach� la inversa de una matriz.

# Esta funci�n crea un objeto "matriz" especial que puede
# almacenar en cach� su inverso.

makeCacheMatrix <- function(m  =  matrix ()) {
    
    i <- NULL
    
    set <- function(matrix) {
      m  <<-  matrix 
      i  <<-  NULL
    }
    get <- function() m
    
    setInv <- function(inv) {
      i <<- inv
    }
    getInv <- function () i
    
    list(set = set,get = get, setInv = setInv, getInv = getInv)
    
  }


# Esta funci�n calcula la inversa de la "matriz" especial
# devuelta por makeCacheMatrix anterior. Si ya se ha calculado
# la inversa (y la matriz no ha cambiado), entonces la soluci�n
# de cach� recupera la inversa de la cach�.

cacheSolve <- function(x, ...) {
  
  m <- x$getInv()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data) %*% data
  
  x$setInv(m)
  
  m
  
}
