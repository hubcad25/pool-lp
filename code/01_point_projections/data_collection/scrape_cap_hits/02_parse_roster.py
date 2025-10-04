"""
Script: Parser le HTML d'un roster pour extraire noms, slugs et cap hits
"""

from bs4 import BeautifulSoup
from pathlib import Path
import pandas as pd

# Fichier HTML sauvegardé
HTML_FILE = Path("code/01_point_projections/data_collection/scrape_cap_hits/mtl_roster.html")

def parse_roster_html(html_path):
    """Parser le HTML pour extraire joueurs et cap hits"""

    html = html_path.read_text(encoding='utf-8')
    soup = BeautifulSoup(html, 'html.parser')

    players = []

    # Trouver toutes les lignes de joueurs (tr avec des td contenant cap hit)
    rows = soup.find_all('tr')

    for row in rows:
        # Chercher le lien du joueur
        player_link = row.find('a', href=lambda x: x and '/player/' in x)
        if not player_link:
            continue

        # Extraire nom et slug
        player_slug = player_link['href'].replace('/player/', '')
        player_name = player_link.get_text(strip=True)

        # Chercher le cap hit dans cette ligne
        cap_td = row.find('td', attrs={'data-extract_ch': True})
        if not cap_td:
            continue

        cap_hit_raw = cap_td.get('data-extract_ch', '0')

        # Convertir en float
        try:
            cap_hit = float(cap_hit_raw.replace(',', ''))
        except:
            cap_hit = 0

        players.append({
            'player_name': player_name,
            'player_slug': player_slug,
            'cap_hit': cap_hit,
            'team': 'MTL'
        })

    return players


def main():
    print(f"Parsing: {HTML_FILE}\n")

    players = parse_roster_html(HTML_FILE)

    print(f"✓ {len(players)} joueurs extraits\n")

    # Créer DataFrame
    df = pd.DataFrame(players)

    # Trier par cap hit
    df = df.sort_values('cap_hit', ascending=False)

    # Afficher
    print("Top 10 cap hits MTL:")
    print(df.head(10).to_string(index=False))

    print(f"\nTotal cap: ${df['cap_hit'].sum():,.0f}")

    return df


if __name__ == "__main__":
    df = main()
