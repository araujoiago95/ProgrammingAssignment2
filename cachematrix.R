## Funções que geram uma matriz inversa 

## Criam uma matriz especial capaz de agrupar a inversa

makeCacheMatrix <- function(x = matrix()) {
  ## Inicializar a propriedade de inversão
  i <- NULL
  
  ## Metodo para dar 'set' na matriz
  set <- function( matrix ) {
      m <<- matrix
      i <<- NULL
  }
  
  ## Metodo para checar a matriz
  get <- function() {
      m
  }
  
  ## Metodo para dar 'set' no inverso da matriz
  setInverse <- function(inverse) {
     i <<- inverse
  }
  
  ## Metodo para gerar matriz inversa
  getInverse <- function() {
    i
  }
  
  ## Retorna uma lista de metodos
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Computa o inverso da matriz especial retornando "makeCacheMatrix" acima. 
## Se o inverso já foi calculado, então o "cachesolve"deve recuperar o inverso do cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## Apenas retorna o inverso se o mesmo já estiver dado "set"
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Pega a matriz do nosso objeto
  data <- x$get()
  
  ## Calcula o inverso usando a multiplicação de matrizes
  m <- solve(data) %*% data
  
  ## Dar "set" no inverso do objeto
  x$setInverse(m)
  
  ## Retorna a matriz
  m
}
