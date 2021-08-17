# -*- coding: utf-8 -*-
# # Utils
# Este archivo recompila las funciones útiles para todos los notebooks realizados en el trabajo práctico

# ## GridSearch

# ### KFold

# Devuelve una lista (de listas) con los indices (del 1 al nrow) agrupados en K grupos.
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

#






