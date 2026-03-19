# PCA vs Kernel PCA: Comparison on the Iris Dataset

Σύγκριση κλασικής Ανάλυσης Κύριων Συνιστωσών (PCA) με Kernel PCA, χρησιμοποιώντας πυρήνες υλοποιημένους from scratch σε R.

## Βασικά Ευρήματα

- Η κλασική PCA διαχωρίζει μόνο τη Setosa — Versicolor και Virginica επικαλύπτονται
- Ο **RBF kernel** (σ = 0.7–1.2) πετυχαίνει τον καλύτερο μη γραμμικό διαχωρισμό
- Ο Polynomial kernel αποδίδει καλά με degree=3, gamma=0.5
- Ο Sigmoid kernel είναι ο πιο ευαίσθητος στην παραμετροποίηση

## Μεθοδολογία

- **Custom kernel functions**: Linear, Polynomial, RBF, Sigmoid — υλοποιημένες χωρίς εξωτερικά πακέτα
- **Kernel matrix centering** σύμφωνα με τη θεωρία (Bishop, 2006)
- **Sensitivity analysis**: Συστηματική δοκιμή παραμέτρων (σ, degree, gamma, coef0)
- **Visualization**: Grid plots για σύγκριση αποτελεσμάτων

## Δομή Αρχείων

```
├── pca_vs_kpca.R          # Κώδικας ανάλυσης (kernels from scratch)
├── report/
│   └── kernel_pca.pdf     # Αναλυτική αναφορά με μαθηματική τεκμηρίωση
└── README.md
```

## Εκτέλεση

```r
# Δεν χρειάζεται εξωτερικό dataset — χρησιμοποιεί το ενσωματωμένο iris
source("pca_vs_kpca.R")
```

## Εργαλεία

R · ggplot2 · gridExtra
