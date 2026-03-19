# Φόρτωση των απαραίτητων βιβλιοθηκών
library(ggplot2)

# Συνάρτηση για υπολογισμό του Γραμμικού Πυρήνα
linear_kernel <- function(X) {
  K <- X %*% t(X)
  return(K)
}

# Συνάρτηση για υπολογισμό του Πολυωνυμικού Πυρήνα
polynomial_kernel <- function(X, degree = 3, gamma = 1, coef0 = 1) {
  K <- (gamma * (X %*% t(X)) + coef0) ^ degree
  return(K)
}

# Συνάρτηση για υπολογισμό του RBF Πυρήνα
rbf_kernel <- function(X, sigma) {
  sq_dist <- as.matrix(dist(X))^2
  K <- exp(-sq_dist / (2 * sigma^2))
  return(K)
}

# Συνάρτηση για υπολογισμό του Υπερβολικού Εφαπτομενικού Πυρήνα
sigmoid_kernel <- function(X, gamma, coef0 = 0) {
  K <- tanh(gamma * (X %*% t(X)) + coef0)
  return(K)
}

# Συνάρτηση για κεντράρισμα του πίνακα πυρήνων
center_kernel <- function(K) {
  n <- nrow(K)
  one_n <- matrix(1 / n, n, n)
  K_centered <- K - one_n %*% K - K %*% one_n + one_n %*% K %*% one_n
  return(K_centered)
}

# Συνάρτηση για εκτέλεση του KPCA
kpca_custom <- function(X, kernel_func, num_components = 2) {
  # Υπολογισμός του πίνακα πυρήνων
  K <- kernel_func(X)
  

  
  # Εκτέλεση του PCA με τη prcomp
  pca_result <- prcomp(K,center=TRUE,scale=FALSE)
  
  # Εξαγωγή των κύριων συνιστωσών
  KPCA_coordinates <- pca_result$x[, 1:num_components]
  
  return(KPCA_coordinates)
}

# Φόρτωση και προετοιμασία των δεδομένων
data(iris)
X <- as.data.frame(iris[, -5])  # Εξαίρεση της στήλης Species
X2 <- scale(X)  # Τυποποίηση δεδομένων



###PCA κανονική
pca_result <- prcomp(Χ2, center = FALSE, scale. = FALSE)

# Δημιουργία dataframe με τα πρώτα δύο κύρια συστατικά
pca_data <- data.frame(PC1 = pca_result$x[,1],
                       PC2 = pca_result$x[,2],
                       Species = labels)

# Βήμα 4: Οπτικοποίηση των Αποτελεσμάτων της PCA
plot_pca <- ggplot(pca_data, aes(x = PC1, y = PC2, color = Species)) +
  geom_point(size = 2) +
  ggtitle("Παραδοσιακή PCA στο Iris Dataset") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(color = "Είδος")

print(plot_pca)


# Εκτέλεση του KPCA με διάφορους πυρήνες
# Γραμμικός Πυρήνας
KPCA_linear <- kpca_custom(X2, linear_kernel, num_components = 2)
KPCA_linear_df <- data.frame(PC1 = KPCA_linear[,1],
                             PC2 = KPCA_linear[,2],
                             Species = iris$Species)

# Πολυωνυμικός Πυρήνας (βαθμός 3, gamma = 1, coef0 = 1)
KPCA_poly <- kpca_custom(X2, function(X) polynomial_kernel(X, degree = 3, gamma = 1, coef0 = 1), num_components = 2)
KPCA_poly_df <- data.frame(PC1 = KPCA_poly[,1],
                           PC2 = KPCA_poly[,2],
                           Species = iris$Species)

# RBF Πυρήνας (συγκεκριμένο sigma = 0.5)
KPCA_rbf <- kpca_custom(X2, function(X) rbf_kernel(X, sigma = 0.01), num_components = 2)
KPCA_rbf_df <- data.frame(PC1 = KPCA_rbf[,1],
                          PC2 = KPCA_rbf[,2],
                          Species = iris$Species)

# Υπερβολικό Εφαπτομενικό Πυρήνας (gamma = 1, coef0 = 0)
KPCA_sigmoid <- kpca_custom(X2, function(X) sigmoid_kernel(X, gamma = 1, coef0 = 0), num_components = 2)
KPCA_sigmoid_df <- data.frame(PC1 = KPCA_sigmoid[,1],
                              PC2 = KPCA_sigmoid[,2],
                              Species = iris$Species)



# RBF Kernel: Δοκιμές για διαφορετικές τιμές sigma
sigma_values <- seq(0.1, 2, by = 0.1)
results_rbf <- list()

for (sigma in sigma_values) {
  KPCA_rbf <- kpca_custom(X2, function(X) rbf_kernel(X, sigma = sigma), num_components = 2)
  results_rbf[[as.character(sigma)]] <- data.frame(PC1 = KPCA_rbf[,1],
                                                   PC2 = KPCA_rbf[,2],
                                                   Species = iris$Species)
}

plots_rbf <- lapply(names(results_rbf), function(sigma) {
  ggplot(results_rbf[[sigma]], aes(x = PC1, y = PC2, color = Species)) +
    geom_point(size = 2) +
    ggtitle(paste("KPCA με RBF Πυρήνα (sigma =", sigma, ")")) +
    theme_minimal()
})

# Polynomial Kernel: Δοκιμές για degree, gamma, και coef0
degree_values <- c(2, 3, 4)
gamma_values <- c(0.1, 0.5, 1, 2)
coef0_values <- c(0, 1, 2)
results_poly <- list()

for (degree in degree_values) {
  for (gamma in gamma_values) {
    for (coef0 in coef0_values) {
      KPCA_poly <- kpca_custom(X2, function(X) polynomial_kernel(X, degree = degree, gamma = gamma, coef0 = coef0), num_components = 2)
      results_poly[[paste("deg", degree, "gamma", gamma, "coef0", coef0, sep = "_")]] <- data.frame(PC1 = KPCA_poly[,1],
                                                                                                    PC2 = KPCA_poly[,2],
                                                                                                    Species = iris$Species)
    }
  }
}

plots_poly <- lapply(names(results_poly), function(key) {
  ggplot(results_poly[[key]], aes(x = PC1, y = PC2, color = Species)) +
    geom_point(size = 2) +
    ggtitle(paste("Polynomial Kernel (", key, ")")) +
    theme_minimal()
})

# Sigmoid Kernel: Δοκιμές για gamma και coef0
gamma_values_sigmoid <- seq(0.1, 2, by = 0.5)
coef0_values_sigmoid <- c(0, 1, 2)
results_sigmoid <- list()

for (gamma in gamma_values_sigmoid) {
  for (coef0 in coef0_values_sigmoid) {
    KPCA_sigmoid <- kpca_custom(X2, function(X) sigmoid_kernel(X, gamma = gamma, coef0 = coef0), num_components = 2)
    results_sigmoid[[paste("gamma", gamma, "coef0", coef0, sep = "_")]] <- data.frame(PC1 = KPCA_sigmoid[,1],
                                                                                      PC2 = KPCA_sigmoid[,2],
                                                                                      Species = iris$Species)
  }
}

plots_sigmoid <- lapply(names(results_sigmoid), function(key) {
  ggplot(results_sigmoid[[key]], aes(x = PC1, y = PC2, color = Species)) +
    geom_point(size = 2) +
    ggtitle(paste("KPCA με Sigmoid Kernel (", key, ")")) +
    theme_minimal()
})

# Εμφάνιση όλων των γραφημάτων
# RBF
grid.arrange(grobs = plots_rbf[1:9], ncol = 3)
grid.arrange(grobs = plots_rbf[10:18], ncol = 3)

# Polynomial
grid.arrange(grobs = plots_poly[1:9], ncol = 3)
grid.arrange(grobs = plots_poly[10:18], ncol = 3)

# Sigmoid
grid.arrange(grobs = plots_sigmoid[1:9], ncol = 3)
grid.arrange(grobs = plots_sigmoid[10:18], ncol = 3)



