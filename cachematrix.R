# Este script almacena en caché la inversa de una matriz.

# Esta función crea un objeto "matriz" especial que puede
# almacenar en caché su inverso.

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


# Esta función calcula la inversa de la "matriz" especial
# devuelta por makeCacheMatrix anterior. Si ya se ha calculado
# la inversa (y la matriz no ha cambiado), entonces la solución
# de caché recupera la inversa de la caché.

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
