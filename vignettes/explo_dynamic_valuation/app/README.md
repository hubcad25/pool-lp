# App Streamlit - Analyse Bayésienne du Shooting %

Application interactive pour visualiser le posterior bayésien du shooting % pour la saison 2024-25.

## Installation

```bash
# Installer les dépendances Python
pip install -r requirements.txt
```

## Lancement

```bash
# Depuis le dossier racine du projet
streamlit run vignettes/explo_dynamic_valuation/app/app.py

# Ou depuis le dossier app/
cd vignettes/explo_dynamic_valuation/app
streamlit run app.py
```

L'app s'ouvrira automatiquement dans votre navigateur à l'adresse `http://localhost:8501`

## Fonctionnalités

### 1. Filtres
- **Minimum de matchs**: Filtrer les joueurs selon un seuil de matchs joués
- **Position**: Filtrer par F (Forwards) ou D (Defensemen)
- **Sélection de joueurs**: Choisir jusqu'à 6 joueurs pour comparaison

### 2. Graphiques

#### Évolution du SH% avec Posterior Bayésien
- **Prior pré-saison** (pointillé): Baseline fixe calculée avec shrinkage
- **Posterior dynamique** (ligne solide): Mise à jour bayésienne match par match
- **Rolling L10** (tirets): Shooting % sur les 10 derniers matchs

#### Détection de Streaks
- Écart entre L10 et Posterior
- **Zone grise (±5%)**: Variation normale
- **Rouge (>+5%)**: Hot streak - performance au-dessus des attentes
- **Bleu (<-5%)**: Cold streak - performance sous les attentes

#### Heatmap (joueur unique)
- Visualisation temporelle des streaks
- Bins de 5 matchs pour réduction de bruit

### 3. Statistiques résumées
- Prior, Posterior final, SH% observé
- Nombre de hot/cold streaks
- Écart moyen L10-Posterior

## Données

L'app charge les données depuis:
```
vignettes/explo_dynamic_valuation/data/game_data_with_posterior.rds
```

Ce fichier contient le posterior bayésien calculé par les scripts `01a` et `01b`.

## Notes techniques

- **Convergence adaptative**: Les recrues (faible historique) convergent plus vite que les vétérans
- **k_posterior**: Varie selon le volume de tirs historique du joueur
- **Formule**: `posterior = obs_weight × sh_observed + (1 - obs_weight) × prior`
