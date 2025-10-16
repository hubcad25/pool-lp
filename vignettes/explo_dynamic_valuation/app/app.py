"""
Streamlit App: Visualisation du Posterior Bayésien pour le Shooting %
Saison 2024-25
"""

import streamlit as st
import pandas as pd
import plotly.graph_objects as go
import plotly.express as px
from plotly.subplots import make_subplots
from pathlib import Path
import pyreadr

# Configuration de la page
st.set_page_config(
    page_title="Shooting % - Analyse Bayésienne",
    page_icon="🏒",
    layout="wide"
)

# Titre
st.title("🏒 Analyse Bayésienne du Shooting % - Saison 2024-25")
st.markdown("Visualisation du prior pré-saison, posterior dynamique et streaks (L5, L10)")

# ============================================
# STEP 1: Charger les données
# ============================================

@st.cache_data
def load_data():
    """Charger les données avec posterior bayésien"""
    data_path = Path(__file__).parent.parent / "data" / "game_data_with_posterior.rds"
    result = pyreadr.read_r(str(data_path))
    df = result[None]  # Premier (et seul) dataframe
    return df

try:
    data = load_data()
    st.sidebar.success(f"✅ Données chargées: {len(data):,} observations")
except Exception as e:
    st.error(f"❌ Erreur lors du chargement des données: {e}")
    st.stop()

# ============================================
# STEP 2: Filtres dans la sidebar
# ============================================

st.sidebar.header("🎯 Filtres")

# Filtrer joueurs avec minimum de matchs
min_games = st.sidebar.slider(
    "Minimum de matchs joués",
    min_value=5,
    max_value=50,
    value=20,
    step=5
)

# Liste des joueurs éligibles
players_eligible = (
    data.groupby(['player_id', 'player_name', 'position'])
    .size()
    .reset_index(name='games')
    .query(f'games >= {min_games}')
    .sort_values('player_name')
)

st.sidebar.markdown(f"**{len(players_eligible)} joueurs éligibles**")

# Filtre par position
positions = st.sidebar.multiselect(
    "Position",
    options=['F', 'D'],
    default=['F', 'D']
)

players_filtered = players_eligible[players_eligible['position'].isin(positions)]

# Sélection des joueurs
selected_players = st.sidebar.multiselect(
    "Sélectionner des joueurs",
    options=players_filtered['player_name'].tolist(),
    default=players_filtered['player_name'].tolist()[:3],  # 3 premiers par défaut
    help="Sélectionnez jusqu'à 6 joueurs pour une meilleure lisibilité"
)

if not selected_players:
    st.warning("⚠️ Veuillez sélectionner au moins un joueur")
    st.stop()

if len(selected_players) > 6:
    st.warning("⚠️ Pour une meilleure lisibilité, limitez la sélection à 6 joueurs maximum")

# Filtrer les données pour les joueurs sélectionnés
df_selected = data[data['player_name'].isin(selected_players)].copy()

# ============================================
# STEP 3: Graphique principal - Évolution SH%
# ============================================

st.header("📈 Évolution du Shooting % avec Posterior Bayésien")

# Calculer disposition de la grille (max 3 colonnes)
n_players = len(selected_players)
n_cols = min(3, n_players)
n_rows = (n_players + n_cols - 1) // n_cols  # Arrondi vers le haut

# Créer subplots avec grille
fig = make_subplots(
    rows=n_rows,
    cols=n_cols,
    subplot_titles=[f"<b>{player}</b>" for player in selected_players],
    shared_yaxes=True,
    horizontal_spacing=0.06,
    vertical_spacing=0.10
)

for i, player in enumerate(selected_players):
    df_player = df_selected[df_selected['player_name'] == player].copy()

    # Calculer position dans la grille
    row_num = (i // n_cols) + 1
    col_num = (i % n_cols) + 1

    # Préparer customdata avec Prior, L5 et L10 pour un hover consolidé
    prior_value = df_player['prior_sh_pct'].iloc[0]
    df_player['prior_value'] = prior_value

    # Prior (ligne horizontale pointillée) - pas de hover
    fig.add_trace(go.Scatter(
        x=df_player['game_index'],
        y=[prior_value] * len(df_player),
        mode='lines',
        name='Prior',
        line=dict(color='#94a3b8', dash='dot', width=2),
        legendgroup='Prior',
        showlegend=(i == 0),
        hoverinfo='skip'  # Skip hover for this trace
    ), row=row_num, col=col_num)

    # Posterior bayésien - hover consolidé avec toutes les stats
    customdata = []
    for _, row in df_player.iterrows():
        l5_val = row['sh_pct_L5'] if row['game_index'] >= 5 and pd.notna(row['sh_pct_L5']) else None
        l10_val = row['sh_pct_L10'] if row['game_index'] >= 10 and pd.notna(row['sh_pct_L10']) else None
        customdata.append([prior_value, l5_val, l10_val])

    fig.add_trace(go.Scatter(
        x=df_player['game_index'],
        y=df_player['sh_pct_posterior'],
        mode='lines',
        name='Posterior',
        line=dict(color='#3b82f6', width=1.5),
        legendgroup='Posterior',
        showlegend=(i == 0),
        customdata=customdata,
        hovertemplate="<b>Match %{x}</b><br>" +
                      "Prior: %{customdata[0]:.1f}%<br>" +
                      "Posterior: %{y:.1f}%<br>" +
                      "L5: %{customdata[1]:.1f}%<br>" +
                      "L10: %{customdata[2]:.1f}%<br>" +
                      "<extra></extra>"
    ), row=row_num, col=col_num)

    # Rolling L5 - pas de hover
    df_L5 = df_player[df_player['game_index'] >= 5].copy()
    if not df_L5.empty:
        fig.add_trace(go.Scatter(
            x=df_L5['game_index'],
            y=df_L5['sh_pct_L5'],
            mode='lines',
            name='L5',
            line=dict(color='#f97316', width=1.5, dash='dot'),
            legendgroup='L5',
            showlegend=(i == 0),
            hoverinfo='skip'  # Skip hover for this trace
        ), row=row_num, col=col_num)

    # Rolling L10 - pas de hover
    df_L10 = df_player[df_player['game_index'] >= 10].copy()
    if not df_L10.empty:
        fig.add_trace(go.Scatter(
            x=df_L10['game_index'],
            y=df_L10['sh_pct_L10'],
            mode='lines',
            name='L10',
            line=dict(color='#10b981', width=1.5, dash='dot'),
            legendgroup='L10',
            showlegend=(i == 0),
            hoverinfo='skip'  # Skip hover for this trace
        ), row=row_num, col=col_num)

# Mise à jour des axes
fig.update_xaxes(title_text="Match #")
fig.update_yaxes(title_text="Shooting %")

fig.update_layout(
    title_text="Prior (pointillé gris) • Posterior Bayésien (bleu) • Rolling L5 (orange) • Rolling L10 (vert)",
    hovermode='closest',
    height=300 * n_rows,
    legend=dict(
        orientation="h",
        yanchor="bottom",
        y=-0.15,
        xanchor="center",
        x=0.5
    )
)

st.plotly_chart(fig, use_container_width=True)

# ============================================
# Footer
# ============================================

st.markdown("---")
st.markdown("""
**Notes méthodologiques:**
- **Prior (gris pointillé)**: Baseline pré-saison calculée avec shrinkage volume-pondéré sur historique 2021-2023
- **Posterior (bleu)**: Mise à jour bayésienne match par match avec convergence adaptative
- **L5 (orange)**: Shooting % sur les 5 derniers matchs (rolling window)
- **L10 (vert)**: Shooting % sur les 10 derniers matchs (rolling window)

Passer la souris sur la ligne bleue pour voir toutes les métriques simultanément.
""")
