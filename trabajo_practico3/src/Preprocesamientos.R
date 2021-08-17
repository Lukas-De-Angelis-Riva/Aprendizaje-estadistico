# -*- coding: utf-8 -*-
# # Preprocesamientos
# En este archivo se nuclearan todas las funciones de preprocesamientos que se realizaran en el trabajo practico.

# # Funcion para means

# +
# Función que recibe un dataframe ('X') y un tamanio de ventana ('tamanio_ventana', 'tv')
# Devuelve otro dataframe donde cada columna es la media de las vecinas
#  en ventanas del tamanio recibido como segundo parametro.

# Por ejemplo:

# X=                       Y=
# A B C D E F G            Y1  Y2  Y3  Y4
# 1 2 1 2 1 2 1    tv=2    1.5 1.5 1.5 1.0
# 2 2 2 2 2 2 2    --->    2.0 2.0 2.0 2.0
# 3 3 4 4 5 5 1            3.0 4.0 5.0 1.0
# 1 2 3 4 5 6 7            1.5 3.5 5.5 7.0

preprocesamiento_agrupar <- function(X, tamanio_ventana) {
  X_means <- data.frame(matrix(NA, nrow = nrow(X), ncol = ceiling(ncol(X) / tamanio_ventana)))
  last_i <- floor(ncol(X) / tamanio_ventana)
  for (i in 1:last_i) {
    j <- i - 1
    X_means[,i] <- rowMeans(X[, (j * tamanio_ventana + 1): (i * tamanio_ventana)])
  }
  num_last_cols <- i * tamanio_ventana + ncol(X) %% tamanio_ventana
  num_first_last_cols <- last_i * tamanio_ventana + 1
  if(num_first_last_cols > num_last_cols){
      return(X_means)
  } else if(num_first_last_cols != num_last_cols) {
    X_means[,ceiling(ncol(X) / tamanio_ventana)] <- rowMeans(X[, num_first_last_cols: num_last_cols])
  } else {
    X_means[,ceiling(ncol(X) / tamanio_ventana)] <- X[, num_first_last_cols]
  }
  return (X_means)
}
