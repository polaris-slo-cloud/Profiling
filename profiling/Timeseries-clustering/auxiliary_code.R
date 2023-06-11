

# Required libraries

library(quantspec)


# Required functions

# Auxiliary function

auxiliary_qcd_function <- function(X, levels = c(0.1, 0.5, 0.9), freq = NULL, type = 'clipped',...) {

  if (is.null(freq)) {

    qSPG <- smoothedPG(X, levels.1 = levels, type = type,...)
    freq <- getFrequencies(qSPG) # Fourier frequencies
    qSPGv <- getValues(qSPG, frequencies = freq)

    matrix <- matrix(qSPGv, ncol = 1)
    quantities <- c(Re(matrix), Im(matrix))
    quantities

  } else {

    qSPG <- smoothedPG(X, levels.1 = levels, type = type, frequencies = freq,...)
    freq <- getFrequencies(qSPG) # Fourier frequencies
    qSPGv <- getValues(qSPG, frequencies = freq)

    matrix <- matrix(qSPGv, ncol = 1)
    quantities <- c(Re(matrix), Im(matrix))
    quantities

  }

}


# Main function

dis_qcd <- function(X, levels = c(0.1, 0.5, 0.9), freq  = NULL, type = 'clipped', features = F,...) {

  l <- length(X)
  c <- ncol(X[[1]])
  series_length <- nrow(X[[1]])
  l_levels <- length(levels)
  n_freq <- ceiling(series_length/2) + 1

  # Feature extraction stage

  matrix_psi <- matrix(nrow = l, ncol = 2*c^2 * l_levels^2 * n_freq)

  for (i in 1 : l) {

    matrix_psi[i,] <- auxiliary_qcd_function(X[[i]], levels = levels, freq = freq, type = type,...)

  }



  if (features == T) {

    matrix_psi

  } else {

    stats::dist(matrix_psi) # Computation of distance matrix

  }




}



