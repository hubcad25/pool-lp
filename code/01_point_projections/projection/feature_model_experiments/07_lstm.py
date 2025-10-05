#!/usr/bin/env python3
# LSTM Model
# Approche: Séquence temporelle avec games_played comme feature

import pandas as pd
import numpy as np
import tensorflow as tf
from tensorflow import keras
from tensorflow.keras import layers
from sklearn.preprocessing import StandardScaler
import pyreadr
import pickle
import os
import warnings
warnings.filterwarnings('ignore')

print("\n=== LSTM Model ===\n")

# Fixer random seeds pour reproducibilité
np.random.seed(42)
tf.random.set_seed(42)

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
    mae = np.mean(np.abs(actual - predicted))

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

# Préparer séquences temporelles -----------------------------------------
print("Préparation des séquences temporelles...")

def prepare_sequences(df_history, feature_name, max_len=3):
    """
    Prépare les séquences temporelles pour LSTM
    Input à chaque timestep: [feature_value, weight, age]
    où weight = recency_weight × (gp / season_length)
    """
    # Longueurs de saison
    season_lengths = {2012: 48, 2020: 56, 2021: 82, 2022: 82, 2023: 82, 2024: 82}

    # Poids de récence
    recency_weights = {0: 0.5, 1: 0.3, 2: 0.2}  # index 0 = t-1, index 1 = t-2, etc.

    sequences = []
    player_ids = []
    positions = []

    for player_id, group in df_history.groupby('player_id'):
        # Trier par saison décroissante (plus récent en premier)
        group = group.sort_values('season', ascending=False)

        # Limiter aux 3 dernières saisons
        group = group.head(max_len)

        # Créer la séquence [feature, weight, age] pour chaque timestep
        sequence = []
        for idx, (_, row) in enumerate(group.iterrows()):
            feature_val = row[feature_name] if pd.notna(row[feature_name]) else 0

            # Calculer weight = recency_weight × (gp / season_length)
            season = int(row['season'])
            gp = row['games_played'] if pd.notna(row['games_played']) else 0
            season_length = season_lengths.get(season, 82)
            recency_weight = recency_weights.get(idx, 0)
            weight_val = recency_weight * (gp / season_length) if gp > 0 else 0

            age_val = row['age'] if pd.notna(row['age']) else 0
            sequence.append([feature_val, weight_val, age_val])

        sequences.append(sequence)
        player_ids.append(player_id)
        positions.append(group.iloc[0]['position'])

    return sequences, player_ids, positions

def pad_sequences_custom(sequences, max_len=3):
    """Pad sequences avec des zéros au début (pour les joueurs avec < 3 saisons)"""
    padded = []
    for seq in sequences:
        if len(seq) < max_len:
            # Pad avec des zéros au début
            padding = [[0, 0, 0]] * (max_len - len(seq))
            padded.append(padding + seq)
        else:
            padded.append(seq)
    return np.array(padded)

# Créer modèle LSTM ------------------------------------------------------
def create_lstm_model(input_shape):
    """
    Architecture: LSTM(64) → Dropout(0.2) → Dense(32) → Output(1)
    """
    model = keras.Sequential([
        layers.Input(shape=input_shape),
        layers.LSTM(64, return_sequences=False),
        layers.Dropout(0.2),
        layers.Dense(32, activation='relu'),
        layers.Dense(1)
    ])

    model.compile(
        optimizer=keras.optimizers.Adam(learning_rate=0.001),
        loss='mse',
        metrics=['mae']
    )

    return model

# Split train data pour validation interne ------------------------------
print("Création du split train/validation interne...")
df_train_internal = df_train[df_train['season'] <= 2022].copy()
df_valid_internal_history = df_train[(df_train['season'] >= 2021) & (df_train['season'] <= 2023)].copy()
df_valid_internal_targets = df_train[df_train['season'] == 2023].copy()

# Entraîner modèle pour chaque feature -----------------------------------
print("\nEntraînement des modèles...\n")

predictions = pd.DataFrame({'player_id': df_valid_targets['player_id']})
metrics_list = []
max_sequence_len = 3

for feature in features_to_project:
    print(f"\n=== Training: {feature} ===")

    # Préparer séquences pour validation interne
    X_train_seq, train_ids, train_pos = prepare_sequences(df_train_internal, feature, max_sequence_len)
    X_valid_seq, valid_ids, valid_pos = prepare_sequences(df_valid_internal_history, feature, max_sequence_len)

    # Pad sequences
    X_train_padded = pad_sequences_custom(X_train_seq, max_sequence_len)
    X_valid_padded = pad_sequences_custom(X_valid_seq, max_sequence_len)

    # Préparer targets
    target_train_df = df_train[df_train['season'] == 2023][['player_id', feature]].dropna(subset=[feature])
    target_valid_df = df_valid_internal_targets[['player_id', feature]].dropna(subset=[feature])

    # Filtrer sequences et créer y aligné avec X
    X_train_list = []
    y_train_list = []
    for i, pid in enumerate(train_ids):
        target_row = target_train_df[target_train_df['player_id'] == pid]
        if len(target_row) > 0:
            X_train_list.append(X_train_padded[i])
            y_train_list.append(target_row[feature].values[0])

    X_valid_list = []
    y_valid_list = []
    for i, pid in enumerate(valid_ids):
        target_row = target_valid_df[target_valid_df['player_id'] == pid]
        if len(target_row) > 0:
            X_valid_list.append(X_valid_padded[i])
            y_valid_list.append(target_row[feature].values[0])

    X_train_filtered = np.array(X_train_list)
    y_train = np.array(y_train_list)
    X_valid_filtered = np.array(X_valid_list)
    y_valid = np.array(y_valid_list)

    # Normaliser les features (important pour LSTM)
    scaler = StandardScaler()
    n_samples_train = X_train_filtered.shape[0]
    n_samples_valid = X_valid_filtered.shape[0]

    X_train_reshaped = X_train_filtered.reshape(-1, 3)
    scaler.fit(X_train_reshaped)

    X_train_scaled = scaler.transform(X_train_reshaped).reshape(n_samples_train, max_sequence_len, 3)
    X_valid_scaled = scaler.transform(X_valid_filtered.reshape(-1, 3)).reshape(n_samples_valid, max_sequence_len, 3)

    # Normaliser aussi les targets
    y_scaler = StandardScaler()
    y_train_scaled = y_scaler.fit_transform(y_train.reshape(-1, 1)).flatten()
    y_valid_scaled = y_scaler.transform(y_valid.reshape(-1, 1)).flatten()

    # Créer et entraîner modèle
    print(f"Entraînement du LSTM (shape: {X_train_scaled.shape})...")
    model = create_lstm_model(input_shape=(max_sequence_len, 3))

    early_stop = keras.callbacks.EarlyStopping(
        monitor='val_loss',
        patience=15,
        restore_best_weights=True
    )

    history = model.fit(
        X_train_scaled, y_train_scaled,
        validation_data=(X_valid_scaled, y_valid_scaled),
        epochs=100,
        batch_size=32,
        callbacks=[early_stop],
        verbose=0
    )

    print(f"Meilleure epoch: {len(history.history['loss']) - 15}")

    # Entraîner sur TOUTES les données train ------------------------------
    print("Réentraînement sur toutes les données train...")

    X_full_seq, full_ids, full_pos = prepare_sequences(df_train, feature, max_sequence_len)
    X_full_padded = pad_sequences_custom(X_full_seq, max_sequence_len)

    # Créer targets (saison la plus récente pour chaque joueur)
    df_train_latest = df_train.sort_values(['player_id', 'season'], ascending=[True, False]).groupby('player_id').first().reset_index()
    target_full_df = df_train_latest[['player_id', feature]].dropna(subset=[feature])

    # Filtrer sequences et créer y aligné avec X
    X_full_list = []
    y_full_list = []
    for i, pid in enumerate(full_ids):
        target_row = target_full_df[target_full_df['player_id'] == pid]
        if len(target_row) > 0:
            X_full_list.append(X_full_padded[i])
            y_full_list.append(target_row[feature].values[0])

    X_full_filtered = np.array(X_full_list)
    y_full = np.array(y_full_list)

    # Normaliser
    scaler_full = StandardScaler()
    n_samples_full = X_full_filtered.shape[0]
    X_full_reshaped = X_full_filtered.reshape(-1, 3)
    scaler_full.fit(X_full_reshaped)
    X_full_scaled = scaler_full.transform(X_full_reshaped).reshape(n_samples_full, max_sequence_len, 3)

    y_scaler_full = StandardScaler()
    y_full_scaled = y_scaler_full.fit_transform(y_full.reshape(-1, 1)).flatten()

    # Créer et entraîner modèle final
    model_final = create_lstm_model(input_shape=(max_sequence_len, 3))

    model_final.fit(
        X_full_scaled, y_full_scaled,
        epochs=len(history.history['loss']) - 15,  # Utiliser le nombre d'epochs optimal
        batch_size=32,
        verbose=0
    )

    # Prédire sur validation ----------------------------------------------
    X_test_seq, test_ids, test_pos = prepare_sequences(df_valid_history, feature, max_sequence_len)
    X_test_padded = pad_sequences_custom(X_test_seq, max_sequence_len)

    n_samples_test = X_test_padded.shape[0]
    X_test_scaled = scaler_full.transform(X_test_padded.reshape(-1, 3)).reshape(n_samples_test, max_sequence_len, 3)

    # Prédictions
    preds_scaled = model_final.predict(X_test_scaled, verbose=0).flatten()
    preds = y_scaler_full.inverse_transform(preds_scaled.reshape(-1, 1)).flatten()

    # Stocker prédictions
    pred_df = pd.DataFrame({
        'player_id': test_ids,
        f'{feature}_pred': preds
    })
    predictions = predictions.merge(pred_df, on='player_id', how='left')

    # Calculer métriques
    eval_df = df_valid_targets[['player_id', feature]].merge(pred_df, on='player_id', how='inner')

    metrics = calculate_metrics(
        eval_df[feature].values,
        eval_df[f'{feature}_pred'].values,
        'lstm',
        feature
    )
    metrics_list.append(metrics)

    print(f"R² = {metrics['r2']:.3f}, MAE = {metrics['mae']:.3f}")

# Créer DataFrame de métriques -------------------------------------------
metrics_df = pd.DataFrame(metrics_list)

# Afficher résultats -----------------------------------------------------
print("\n=== Résultats LSTM ===\n")
print(metrics_df.to_string(index=False))

# Sauvegarder ------------------------------------------------------------
output_dir = "data/01_point_projections/projection/experiments/results/lstm"
os.makedirs(output_dir, exist_ok=True)

# Sauvegarder en pickle
with open(f"{output_dir}/metrics.pkl", "wb") as f:
    pickle.dump(metrics_df, f)

with open(f"{output_dir}/predictions.pkl", "wb") as f:
    pickle.dump(predictions, f)

# Sauvegarder en CSV
metrics_df.to_csv(f"{output_dir}/metrics.csv", index=False)
predictions.to_csv(f"{output_dir}/predictions.csv", index=False)

# Sauvegarder en RDS pour compatibilité avec R
try:
    import rpy2.robjects as ro
    from rpy2.robjects import pandas2ri
    pandas2ri.activate()

    ro.r('saveRDS')(pandas2ri.py2rpy(metrics_df), f"{output_dir}/metrics.rds")
    ro.r('saveRDS')(pandas2ri.py2rpy(predictions), f"{output_dir}/predictions.rds")
    print("\n✓ Résultats sauvegardés en RDS et CSV dans data/.../results/lstm/\n")
except ImportError:
    print("\n✓ Résultats sauvegardés en CSV et pickle dans data/.../results/lstm/")
    print("  (rpy2 non disponible pour sauvegarder en RDS)\n")
