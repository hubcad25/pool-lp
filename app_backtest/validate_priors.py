import streamlit as st
import pandas as pd
import pyreadr
from pathlib import Path

# Configuration de la page
st.set_page_config(
    page_title="Validation Priors 2024-25",
    page_icon="🏒",
    layout="wide"
)

@st.cache_data
def load_data():
    """Charge les données RDS des priors de backtesting"""
    current_dir = Path(__file__).parent
    data_path = current_dir.parent / 'data' / '03_dynamic_valuation' / 'priors_2025.rds'
    
    if not data_path.exists():
        st.error(f"Fichier de données introuvable: {data_path}")
        return None
        
    result = pyreadr.read_r(str(data_path))
    df = result[None]
    return df

# Chargement des données
df = load_data()

if df is not None:
    # Titre
    st.title("🏒 Validation des Priors pour Backtesting (2024-25)")
    st.markdown("---")

    # Sidebar pour les filtres
    st.sidebar.header("Filtres")

    # Filtre scénario
    scenarios = ['mid', 'high', 'low']
    selected_scenario = st.sidebar.selectbox(
        "Scénario",
        scenarios,
        index=0,
        help="mid = médian, high = optimiste (P90), low = pessimiste (P10)"
    )

    # Filtrer par scénario
    df_filtered = df[df['scenario'] == selected_scenario].copy()

    # Recherche par nom
    search_name = st.sidebar.text_input("Rechercher un joueur", "")
    if search_name:
        df_filtered = df_filtered[
            df_filtered['full_name'].str.contains(search_name, case=False, na=False)
        ]

    st.sidebar.metric("Joueurs affichés", len(df_filtered))

    # Affichage principal
    st.header(f"Projections - Scénario '{selected_scenario.capitalize()}'")

    # Préparer le tableau d'affichage
    display_df = df_filtered[[
        'full_name', 'position', 'team',
        'goals', 'assists', 'points'
    ]].copy()

    # Trier par points décroissants
    display_df = display_df.sort_values('points', ascending=False)

    # Renommer colonnes pour affichage
    display_df.columns = ['Joueur', 'Pos', 'Équipe', 'Buts', 'Passes', 'Points']

    # Afficher le tableau
    st.dataframe(
        display_df.style.format({
            'Buts': '{:.1f}',
            'Passes': '{:.1f}',
            'Points': '{:.1f}'
        }).background_gradient(subset=['Buts', 'Passes', 'Points'], cmap='viridis'),
        height=700,
        use_container_width=True
    )

    # Bouton téléchargement CSV
    csv = display_df.to_csv(index=False)
    st.download_button(
        "📥 Télécharger en CSV",
        csv,
        f"priors_2025_{selected_scenario}.csv",
        "text/csv"
    )
else:
    st.warning("Le chargement des données a échoué. Assurez-vous que le fichier `priors_2025.rds` existe.")
