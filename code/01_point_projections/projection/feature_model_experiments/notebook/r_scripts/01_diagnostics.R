# 01_diagnostics.R - Vérifications et diagnostics

cat("\n=== DIAGNOSTICS ===\n\n")

# NAs par colonne
cat("NAs par colonne:\n")
print(colSums(is.na(all_metrics)))
cat("\n")

# R² négatifs
cat("R² négatifs (modèles pires que baseline naïve):\n")
negative_r2 <- all_metrics %>%
  filter(r2 < 0) %>%
  select(model, feature, r2, mae) %>%
  arrange(r2)
print(negative_r2, row.names = FALSE)
cat("\n")

# R² suspects (>1 ou <-1)
cat("R² suspects (>1 ou <-1):\n")
suspect_r2 <- all_metrics %>%
  filter(r2 > 1 | r2 < -1) %>%
  select(model, feature, r2, mae)
if (nrow(suspect_r2) > 0) {
  print(suspect_r2, row.names = FALSE)
} else {
  cat("Aucun R² suspect\n")
}
cat("\n")

# Performance moyenne par modèle
cat("Performance moyenne par modèle:\n")
perf_by_model <- all_metrics %>%
  filter(!is.na(r2)) %>%
  group_by(model) %>%
  summarise(
    mean_r2 = mean(r2, na.rm = TRUE),
    median_r2 = median(r2, na.rm = TRUE),
    min_r2 = min(r2, na.rm = TRUE),
    max_r2 = max(r2, na.rm = TRUE),
    sd_r2 = sd(r2, na.rm = TRUE),
    n_features = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_r2))
print(perf_by_model, row.names = FALSE, digits = 3)
cat("\n")

# Performance moyenne par feature
cat("Performance moyenne par feature:\n")
perf_by_feature <- all_metrics %>%
  filter(!is.na(r2)) %>%
  group_by(feature) %>%
  summarise(
    mean_r2 = mean(r2, na.rm = TRUE),
    best_r2 = max(r2, na.rm = TRUE),
    worst_r2 = min(r2, na.rm = TRUE),
    best_model = model[which.max(r2)],
    n_models = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(best_r2))
print(perf_by_feature, row.names = FALSE, digits = 3)
cat("\n")

# Identifier meilleur modèle par feature
best_by_feature <- all_metrics %>%
  filter(!is.na(r2)) %>%
  group_by(feature) %>%
  slice_max(r2, n = 1) %>%
  ungroup()

cat("Meilleurs modèles par feature:\n")
print(best_by_feature %>% select(feature, model, r2, mae), row.names = FALSE, digits = 3)
