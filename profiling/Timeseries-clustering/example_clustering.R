
library(quantspec)
library(ClusterR)
library(cluster)

source('auxiliary_code.R')
# Loading the data (25 MTS from 5 different clusters)

load('list_mts.RData')

# Performing clustering over the list of MTS via the PAM algorithm

# Computing the distance matrix

distance_matrix <- dis_qcd(list_mts)

# PAM algorithm for 5 clusters

clustering_pam <- pam(distance_matrix, k = 5)

# Printing the resulting partition

clustering_solution <- clustering_pam$clustering
clustering_solution

# Computing the value of the Adjusted Rand Index

ground_truth <- c(rep(1, 5), rep(2, 5), rep(3, 5), rep(4, 5), rep(5, 5)) # Ground truth
external_validation(ground_truth, clustering_solution)

