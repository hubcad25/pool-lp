import streamlit as st
import pandas as pd
import pyreadr
import plotly.express as px
from pathlib import Path

# Configuration de la page
st.set_page_config(
    page_title="Projections Hockey 2025-26",
    page_icon="🏒",
    layout="wide"
)

@st.cache_data
def load_data():
    """Charge les données RDS"""
    # Chemin relatif à partir du fichier actuel
    current_dir = Path(__file__).parent
    data_path = current_dir.parent / 'data' / '01_point_projections' / 'projection' / 'projections_2026_final.rds'
    result = pyreadr.read_r(str(data_path))
    df = result[None]  # pyreadr retourne un dict, None est la clé par défaut
    return df

# Chargement des données
try:
    df = load_data()

    # Titre
    st.title("🏒 Projections Points 2025-26")
    st.markdown("---")

    # Sidebar pour les filtres
    st.sidebar.header("Filtres")

    # Filtre scénario
    scenarios = ['mid', 'high', 'low']
    selected_scenario = st.sidebar.selectbox(
        "Scénario",
        scenarios,
        index=0,
        help="mid = médian, high = optimiste (90e percentile), low = pessimiste (10e percentile)"
    )

    # Filtrer par scénario
    df_filtered = df[df['scenario'] == selected_scenario].copy()

    # Filtre équipe
    teams = sorted(df_filtered['team'].unique())
    selected_teams = st.sidebar.multiselect(
        "Équipe(s)",
        teams,
        default=None,
        help="Laisser vide pour toutes les équipes"
    )

    if selected_teams:
        df_filtered = df_filtered[df_filtered['team'].isin(selected_teams)]

    # Filtre position
    positions = sorted(df_filtered['position'].unique())
    selected_positions = st.sidebar.multiselect(
        "Position(s)",
        positions,
        default=None,
        help="C=Centre, L=Ailier gauche, R=Ailier droit, D=Défenseur"
    )

    if selected_positions:
        df_filtered = df_filtered[df_filtered['position'].isin(selected_positions)]

    # Filtre âge
    min_age, max_age = int(df_filtered['age'].min()), int(df_filtered['age'].max())
    age_range = st.sidebar.slider(
        "Âge",
        min_age, max_age,
        (min_age, max_age)
    )
    df_filtered = df_filtered[(df_filtered['age'] >= age_range[0]) & (df_filtered['age'] <= age_range[1])]

    # Filtre points minimum
    min_points = st.sidebar.number_input(
        "Points minimum",
        min_value=0,
        max_value=int(df_filtered['points'].max()),
        value=0,
        step=5
    )
    df_filtered = df_filtered[df_filtered['points'] >= min_points]

    # Recherche par nom
    search_name = st.sidebar.text_input("Rechercher un joueur", "")
    if search_name:
        df_filtered = df_filtered[
            df_filtered['first_name'].str.contains(search_name, case=False, na=False) |
            df_filtered['last_name'].str.contains(search_name, case=False, na=False)
        ]

    # Métriques principales
    st.sidebar.markdown("---")
    st.sidebar.metric("Joueurs affichés", len(df_filtered))

    # Affichage principal
    col1, col2, col3, col4 = st.columns(4)

    with col1:
        st.metric("Points moyens", f"{df_filtered['points'].mean():.1f}")
    with col2:
        st.metric("Buts moyens", f"{df_filtered['goals'].mean():.1f}")
    with col3:
        st.metric("Passes moyennes", f"{df_filtered['assists'].mean():.1f}")
    with col4:
        st.metric("Salaire moyen", f"${df_filtered['cap_hit'].mean()/1e6:.2f}M")

    st.markdown("---")

    # Onglets
    tab1, tab2, tab3 = st.tabs(["📊 Tableau", "📈 Graphiques", "🔍 Détails"])

    with tab1:
        # Préparer le tableau d'affichage
        display_df = df_filtered[[
            'first_name', 'last_name', 'position', 'team', 'age',
            'goals', 'assists', 'points', 'cap_hit'
        ]].copy()

        display_df['player'] = display_df['first_name'] + ' ' + display_df['last_name']
        display_df['cap_hit_m'] = display_df['cap_hit'] / 1e6

        # Trier par points décroissants
        display_df = display_df.sort_values('points', ascending=False)

        # Renommer colonnes pour affichage
        display_df = display_df[[
            'player', 'position', 'team', 'age', 'goals', 'assists', 'points', 'cap_hit_m'
        ]]
        display_df.columns = ['Joueur', 'Pos', 'Équipe', 'Âge', 'Buts', 'Passes', 'Points', 'Salaire (M$)']

        # Formater les colonnes numériques
        st.dataframe(
            display_df.style.format({
                'Buts': '{:.1f}',
                'Passes': '{:.1f}',
                'Points': '{:.1f}',
                'Salaire (M$)': '${:.2f}'
            }).background_gradient(subset=['Buts', 'Passes', 'Points'], cmap='YlOrRd'),
            height=600,
            use_container_width=True
        )

        # Bouton téléchargement CSV
        csv = display_df.to_csv(index=False)
        st.download_button(
            "📥 Télécharger CSV",
            csv,
            f"projections_2026_{selected_scenario}.csv",
            "text/csv"
        )

    with tab2:
        col1, col2 = st.columns(2)

        with col1:
            # Scatter plot Buts vs Passes
            fig1 = px.scatter(
                df_filtered,
                x='goals',
                y='assists',
                color='position',
                size='points',
                hover_data=['first_name', 'last_name', 'team', 'age'],
                title='Buts vs Passes',
                labels={'goals': 'Buts projetés', 'assists': 'Passes projetées'}
            )
            st.plotly_chart(fig1, use_container_width=True)

            # Distribution des points par position
            fig3 = px.box(
                df_filtered,
                x='position',
                y='points',
                color='position',
                title='Distribution des points par position',
                labels={'position': 'Position', 'points': 'Points projetés'}
            )
            st.plotly_chart(fig3, use_container_width=True)

        with col2:
            # Top 20 joueurs
            top20 = df_filtered.nlargest(20, 'points').copy()
            top20['player'] = top20['first_name'] + ' ' + top20['last_name']

            fig2 = px.bar(
                top20,
                x='points',
                y='player',
                orientation='h',
                color='position',
                title='Top 20 - Points projetés',
                labels={'points': 'Points', 'player': ''}
            )
            fig2.update_layout(yaxis={'categoryorder': 'total ascending'})
            st.plotly_chart(fig2, use_container_width=True)

            # Relation points vs salaire
            fig4 = px.scatter(
                df_filtered,
                x='cap_hit',
                y='points',
                color='position',
                hover_data=['first_name', 'last_name', 'team'],
                title='Points vs Salaire',
                labels={'cap_hit': 'Salaire ($)', 'points': 'Points projetés'}
            )
            fig4.update_xaxis(tickformat='$,.0f')
            st.plotly_chart(fig4, use_container_width=True)

    with tab3:
        st.subheader("Statistiques détaillées")

        # Sélectionner un joueur
        players_list = (df_filtered['first_name'] + ' ' + df_filtered['last_name'] + ' (' + df_filtered['team'] + ')').sort_values().tolist()

        if players_list:
            selected_player_full = st.selectbox("Sélectionner un joueur", players_list)

            # Extraire le nom
            selected_player_name = selected_player_full.split(' (')[0]
            first_name, last_name = selected_player_name.split(' ', 1)

            # Afficher les 3 scénarios pour ce joueur
            player_data = df[
                (df['first_name'] == first_name) &
                (df['last_name'] == last_name)
            ].sort_values('scenario', key=lambda x: x.map({'low': 0, 'mid': 1, 'high': 2}))

            if not player_data.empty:
                st.markdown(f"### {selected_player_name}")

                col1, col2, col3, col4 = st.columns(4)
                with col1:
                    st.metric("Position", player_data.iloc[0]['position'])
                with col2:
                    st.metric("Équipe", player_data.iloc[0]['team'])
                with col3:
                    st.metric("Âge", int(player_data.iloc[0]['age']))
                with col4:
                    st.metric("Salaire", f"${player_data.iloc[0]['cap_hit']/1e6:.2f}M")

                st.markdown("#### Scénarios de projection")

                scenarios_df = player_data[['scenario', 'goals', 'assists', 'points']].copy()
                scenarios_df = scenarios_df.sort_values('scenario', key=lambda x: x.map({'low': 0, 'mid': 1, 'high': 2}))
                scenarios_df.columns = ['Scénario', 'Buts', 'Passes', 'Points']

                st.dataframe(
                    scenarios_df.style.format({
                        'Buts': '{:.1f}',
                        'Passes': '{:.1f}',
                        'Points': '{:.1f}'
                    }),
                    use_container_width=True,
                    hide_index=True
                )

                # Graphique de comparaison des scénarios
                fig = px.bar(
                    scenarios_df,
                    x='Scénario',
                    y=['Buts', 'Passes'],
                    title='Comparaison des scénarios',
                    barmode='group'
                )
                st.plotly_chart(fig, use_container_width=True)

                # Statistiques avancées
                st.markdown("#### Statistiques avancées")
                advanced_cols = [
                    'evtoi_per_gp', 'pptoi_per_gp', 'high_danger_shots',
                    'conversion_high_danger', 'conversion_overall', 'x_goals'
                ]

                # Sélectionner le scénario mid pour les stats avancées
                player_mid = player_data[player_data['scenario'] == 'mid'].iloc[0]

                col1, col2, col3 = st.columns(3)
                with col1:
                    st.metric("TOI 5v5 (sec/match)", f"{player_mid['evtoi_per_gp']:.0f}")
                    st.metric("TOI PP (sec/match)", f"{player_mid['pptoi_per_gp']:.0f}")
                with col2:
                    st.metric("Tirs haute danger", f"{player_mid['high_danger_shots']:.1f}")
                    st.metric("Expected goals", f"{player_mid['x_goals']:.1f}")
                with col3:
                    st.metric("Conversion HD", f"{player_mid['conversion_high_danger']:.1%}")
                    st.metric("Conversion totale", f"{player_mid['conversion_overall']:.1%}")

except FileNotFoundError:
    st.error("❌ Fichier de données introuvable: `data/01_point_projections/projection/projections_2026_final.rds`")
except Exception as e:
    st.error(f"❌ Erreur lors du chargement des données: {str(e)}")
    st.info("Assurez-vous que le package `pyreadr` est installé: `pip install pyreadr`")
