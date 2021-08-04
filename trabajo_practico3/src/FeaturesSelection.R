vasijas_X <- read.csv("Vessel_X.txt", header = FALSE)
vasijas_Y <- read.csv("Vessel_Y.txt", header = FALSE)
oxido_sodio_Y <- c(vasijas_Y$V1)

merged_vasijas <- vasijas_X
merged_vasijas$Y <- oxido_sodio_Y

# No tiene elementos nulos
sum(is.nan(as.matrix(merged_vasijas)))

# Elimina las variables con correlacion mayor a max_corr_acepted
# Retorna el dataframe con las variables nuevas. 
eliminate_variables_high_corr <- function(vasijas_X, max_corr_acepted) {
  new_vasijas_X <- vasijas_X
  are_variables_high_corr <- TRUE
  while (are_variables_high_corr) {
    corr_matrix <- cor(new_vasijas_X, method = "pearson")
    i <- 2
    loop_not_finished <- TRUE
    while (i <= ncol(new_vasijas_X) && loop_not_finished) {
      j <- 1
      while (j <= (i-1) && loop_not_finished) {
        if (corr_matrix[i, j] >= max_corr_acepted) {
          new_vasijas_X <- subset(new_vasijas_X, select = -i)
          loop_not_finished <- FALSE
        }
        if (i == nrow(corr_matrix) && j == nrow(corr_matrix) - 1) {
          are_variables_high_corr <- FALSE
        }
        j <- j + 1
      }
      i <- i + 1
    }
  }
  return(new_vasijas_X)
}

new_vasijas_X <- eliminate_variables_high_corr(vasijas_X, 0.999)

write.csv(new_vasijas_X, "VesselSelectedFeatures_X.csv")
