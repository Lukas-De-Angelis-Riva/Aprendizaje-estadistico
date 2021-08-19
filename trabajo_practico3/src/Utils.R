# -*- coding: utf-8 -*-
# # Utils
# Este archivo recompila las funciones útiles para todos los notebooks realizados en el trabajo práctico

# ### Semilla

setSeed <- function(){
    set.seed(5)
}

# ### KFold

# Devuelve una lista (de doubles) con los indices (del 1 al nrow) agrupados en K grupos.
# No es necesario que nrow sea multiplo de k, en caso de que sobren indices se agruparan en los primeros grupos
#  hasta que se terminen de agrupar todos.
obtener_indices_kfold <- function(nrow, k){
    indices <- list()
    for(j in 1:k) indices[[j]] <- as.double(list())
    desordenados <- sample(seq(1, nrow))
    
    for(i in 1:nrow){
        j <- (i-1)%%k + 1
        indices[[j]] <- c(indices[[j]], desordenados[i])
    }
    return(indices)
}

# ### Print

# Devuelve un string enunciando el progreso por haber terminado de iterar el valor 'v'
#  entre todos los valores en el vector 'valores'.
# En caso de que 'v' no pertenezca a 'valores' entonces se devolverá '???'. 
# Si 'v' se encuentra en valores, se devolverá un string con el siguiente formato:
# "El progreso es del p%"'
progreso <- function(v, valores, digits=1){
    i <- match(v, valores)
    if(is.na(i)) return("???")
    p = round(i/length(valores)*100, digits)
    return(paste("El progreso es del", p, "%"))
}

# ### Datos

obtener_X <- function(){
    return(read.csv("Vessel_X.txt", header = FALSE))
}
obtener_Y <- function(){
    vasijas_Y <- read.csv("Vessel_Y.txt", header = FALSE)
    return(c(vasijas_Y$V1))
}
# Es necesario haber establecido una semilla antes de llamar a esta funcion,
#  pues sino dara diferentes resultados cada llamado.
obtener_holdout_ind <- function(X){
    return(sample(seq_len(nrow(X)), size = 20))
}






