#!/usr/bin/env python3
# XGBoost Model
# Approche: Gradient boosting avec games_played comme FEATURE

import pandas as pd
import numpy as np
import xgboost as xgb
from sklearn.model_selection import GridSearchCV
from sklearn.metrics import mean_absolute_error, mean_absolute_percentage_error
import pyreadr
import pickle
import os

print("\n=== XGBoost Model ===\n")

# Charger données --------------------------------------------------------
print("Chargement des données...")
df_train = pyreadr.read_r("data/01_point_projections/projection/experiments/validation_data/df_train.rds")[None]
df_valid_history = pyreadr.read_r("data/01_point_projections/projection/experiments/validation_data/df_valid_history.rds")[None]
df_valid_targets = pyreadr.read_r("data/01_point_projections/projection/experiments/validation_data/df_valid_targets.rds")[None]

# Features à projeter ----------------------------------------------------
features_to_project = [
    "evtoi_per_gp", "pptoi_per_gp",
    "high_danger_shots_per60", "medium_danger_shots_per60",
    "x_goals_per60", "shot_attempts_per60",
    "conversion_high_danger", "conversion_medium", "conversion_overall"
]

# Fonction calculate_metrics ---------------------------------------------
def calculate_metrics(actual, predicted, model_name, feature_name):
    # Retirer NAs et valeurs infinies
    valid_idx = ~(pd.isna(actual) | pd.isna(predicted) |
                  np.isinf(actual) | np.isinf(predicted))
    actual = actual[valid_idx]
    predicted = predicted[valid_idx]

    if len(actual) < 10:
        return {
            'model': model_name,
            'feature': feature_name,
            'r2': np.nan,
            'mae': np.nan,
            'mape': np.nan,
            'n': len(actual)
        }

    # R²
    ss_res = np.sum((actual - predicted) ** 2)
    ss_tot = np.sum((actual - actual.mean()) ** 2)
    r2 = 1 - ss_res / ss_tot

    # MAE
    mae = mean_absolute_error(actual, predicted)

    # MAPE
    mape_values = np.abs((actual - predicted) / (actual + 1e-10)) * 100
    mape = np.mean(mape_values[np.isfinite(mape_values)])

    return {
        'model': model_name,
        'feature': feature_name,
        'r2': r2,
        'mae': mae,
        'mape': mape,
        'n': len(actual)
    }

# Préparer données avec historique pivoté -------------------------------
print("Préparation des données avec historique pivoté...")

def pivot_history(df_history, feature_name):
    """Pivot history to wide format with t-1, t-2, t-3 and calculate weights"""
    df = df_history.copy()

    # Longueurs de saison
    season_lengths = {2012: 48, 2020: 56, 2021: 82, 2022: 82, 2023: 82, 2024: 82}

    # Poids de récence
    recency_weights = {1: 0.5, 2: 0.3, 3: 0.2}

    # Trier par player_id et season décroissant
    df = df.sort_values(['player_id', 'season'], ascending=[True, False])

    # Ajouter rang pour chaque joueur
    df['time_lag'] = df.groupby('player_id').cumcount() + 1

    # Garder seulement les 3 dernières saisons
    df = df[df['time_lag'] <= 3]

    # Calculer weight = recency_weight × (gp / season_length)
    df['season_length'] = df['season'].map(season_lengths)
    df['recency_weight'] = df['time_lag'].map(recency_weights)
    df['weight_val'] = df['recency_weight'] * (df['games_played'] / df['season_length'])

    # Créer colonnes pour feature et weight
    df['feature_val'] = df[feature_name]

    # Pivot pour features
    df_feature = df.pivot(index='player_id', columns='time_lag', values='feature_val')
    df_feature.columns = [f'{feature_name}_t{i}' for i in df_feature.columns]

    # Pivot pour weights
    df_weight = df.pivot(index='player_id', columns='time_lag', values='weight_val')
    df_weight.columns = [f'weight_t{i}' for i in df_weight.columns]

    # Joindre
    df_result = df_feature.join(df_weight)

    # Ajouter age et position (prendre la plus récente = time_lag 1)
    df_meta = df[df['time_lag'] == 1][['player_id', 'age', 'position']].set_index('player_id')
    df_result = df_result.join(df_meta)

    return df_result.reset_index()

# Split train data en train/valid pour early stopping -------------------
print("Création du split train/validation interne...")
df_train_internal = df_train[df_train['season'] <= 2022].copy()
df_valid_internal_history = df_train[(df_train['season'] >= 2021) & (df_train['season'] <= 2023)].copy()
df_valid_internal_targets = df_train[df_train['season'] == 2023].copy()

# Hyperparamètres à tester -----------------------------------------------
param_grid = {
    'max_depth': [3, 5, 7],
    'learning_rate': [0.01, 0.05, 0.1],
    'subsample': [0.7, 0.8],
    'n_estimators': [100, 200, 300]
}

# Entraîner modèle pour chaque feature -----------------------------------
print("\nEntraînement des modèles...\n")

predictions = pd.DataFrame({'player_id': df_valid_targets['player_id']})
metrics_list = []
best_params = {}

for feature in features_to_project:
    print(f"\n=== Training: {feature} ===")

    # Préparer données pivotées pour validation interne
    df_train_pivot_internal = pivot_history(df_train_internal, feature)
    df_valid_pivot_internal = pivot_history(df_valid_internal_history, feature)

    # Joindre avec targets
    df_train_full_internal = df_train_pivot_internal.merge(
        df_train[df_train['season'] == 2023][['player_id', feature]].rename(columns={feature: 'target'}),
        on='player_id',
        how='inner'
    ).dropna(subset=['target'])

    df_valid_full_internal = df_valid_pivot_internal.merge(
        df_valid_internal_targets[['player_id', feature]].rename(columns={feature: 'target'}),
        on='player_id',
        how='inner'
    ).dropna(subset=['target'])

    # Features à utiliser
    feature_cols = [
        f'{feature}_t1', f'{feature}_t2', f'{feature}_t3',
        'weight_t1', 'weight_t2', 'weight_t3',
        'age'
    ]

    # Encoder position (F=1, D=0)
    df_train_full_internal['pos_encoded'] = df_train_full_internal['position'].apply(
        lambda x: 1 if x in ['C', 'L', 'R'] else 0
    )
    df_valid_full_internal['pos_encoded'] = df_valid_full_internal['position'].apply(
        lambda x: 1 if x in ['C', 'L', 'R'] else 0
    )

    feature_cols.append('pos_encoded')

    # Remplacer NAs par 0 pour les saisons manquantes
    df_train_full_internal[feature_cols] = df_train_full_internal[feature_cols].fillna(0)
    df_valid_full_internal[feature_cols] = df_valid_full_internal[feature_cols].fillna(0)

    # Préparer X, y
    X_train_internal = df_train_full_internal[feature_cols]
    y_train_internal = df_train_full_internal['target']

    X_valid_internal = df_valid_full_internal[feature_cols]
    y_valid_internal = df_valid_full_internal['target']

    # Grid search avec validation interne
    print("Hyperparameter tuning...")
    xgb_model = xgb.XGBRegressor(
        objective='reg:squarederror',
        random_state=42,
        n_jobs=-1
    )

    grid_search = GridSearchCV(
        xgb_model,
        param_grid,
        cv=3,
        scoring='r2',
        n_jobs=-1,
        verbose=0
    )

    grid_search.fit(X_train_internal, y_train_internal)

    print(f"Meilleurs params: {grid_search.best_params_}")
    print(f"Meilleur score CV: {grid_search.best_score_:.3f}")

    best_params[feature] = grid_search.best_params_

    # Entraîner sur TOUTES les données train avec meilleurs params ----------
    df_train_pivot_full = pivot_history(df_train, feature)
    df_valid_pivot = pivot_history(df_valid_history, feature)

    # Encoder position
    df_train_pivot_full['pos_encoded'] = df_train_pivot_full['position'].apply(
        lambda x: 1 if x in ['C', 'L', 'R'] else 0
    )
    df_valid_pivot['pos_encoded'] = df_valid_pivot['position'].apply(
        lambda x: 1 if x in ['C', 'L', 'R'] else 0
    )

    # Remplacer NAs
    df_train_pivot_full[feature_cols] = df_train_pivot_full[feature_cols].fillna(0)
    df_valid_pivot[feature_cols] = df_valid_pivot[feature_cols].fillna(0)

    # Joindre avec target (prendre la saison la plus récente pour chaque joueur)
    df_train_latest = df_train.sort_values(['player_id', 'season'], ascending=[True, False]).groupby('player_id').first().reset_index()
    df_train_final = df_train_pivot_full.merge(
        df_train_latest[['player_id', feature]].rename(columns={feature: 'target'}),
        on='player_id',
        how='inner'
    ).dropna(subset=['target'])

    X_train_full = df_train_final[feature_cols]
    y_train_full = df_train_final['target']

    # Entraîner modèle final avec meilleurs params
    xgb_final = xgb.XGBRegressor(
        **grid_search.best_params_,
        objective='reg:squarederror',
        random_state=42,
        n_jobs=-1
    )
    xgb_final.fit(X_train_full, y_train_full)

    # Prédire sur validation
    X_valid = df_valid_pivot[feature_cols]
    preds = xgb_final.predict(X_valid)

    # Stocker prédictions
    pred_df = pd.DataFrame({
        'player_id': df_valid_pivot['player_id'],
        f'{feature}_pred': preds
    })
    predictions = predictions.merge(pred_df, on='player_id', how='left')

    # Calculer métriques
    eval_df = df_valid_targets[['player_id', feature]].merge(pred_df, on='player_id', how='inner')

    metrics = calculate_metrics(
        eval_df[feature].values,
        eval_df[f'{feature}_pred'].values,
        'xgboost',
        feature
    )
    metrics_list.append(metrics)

# Créer DataFrame de métriques -------------------------------------------
metrics_df = pd.DataFrame(metrics_list)

# Afficher résultats -----------------------------------------------------
print("\n=== Résultats XGBoost ===\n")
print(metrics_df.to_string(index=False))

# Sauvegarder ------------------------------------------------------------
output_dir = "data/01_point_projections/projection/experiments/results/xgboost"
os.makedirs(output_dir, exist_ok=True)

# Sauvegarder en pickle (Python native)
with open(f"{output_dir}/metrics.pkl", "wb") as f:
    pickle.dump(metrics_df, f)

with open(f"{output_dir}/predictions.pkl", "wb") as f:
    pickle.dump(predictions, f)

with open(f"{output_dir}/best_params.pkl", "wb") as f:
    pickle.dump(best_params, f)

# Sauvegarder aussi en CSV pour compatibilité R
metrics_df.to_csv(f"{output_dir}/metrics.csv", index=False)
predictions.to_csv(f"{output_dir}/predictions.csv", index=False)

# Sauvegarder en RDS pour compatibilité avec R
try:
    import rpy2.robjects as ro
    from rpy2.robjects import pandas2ri
    pandas2ri.activate()

    ro.r('saveRDS')(pandas2ri.py2rpy(metrics_df), f"{output_dir}/metrics.rds")
    ro.r('saveRDS')(pandas2ri.py2rpy(predictions), f"{output_dir}/predictions.rds")
    print("\n✓ Résultats sauvegardés en RDS et CSV dans data/.../results/xgboost/\n")
except ImportError:
    print("\n✓ Résultats sauvegardés en CSV et pickle dans data/.../results/xgboost/")
    print("  (rpy2 non disponible pour sauvegarder en RDS)\n")
