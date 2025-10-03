"""
Script: Scraper les lineups de Puckpedia
Source: https://puckpedia.com/lineups/{team}
Output: data/01_point_projections/lineups/puckpedia_raw.csv
"""

import pandas as pd
from playwright.async_api import async_playwright
from bs4 import BeautifulSoup
import asyncio
import time
from pathlib import Path

# Configuration
OUTPUT_DIR = Path("data/01_point_projections/lineups")
OUTPUT_FILE = OUTPUT_DIR / "puckpedia_raw.csv"

# Créer le dossier si nécessaire
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

# Équipes NHL (32 équipes) - Noms extraits de https://puckpedia.com/lineups
NHL_TEAMS = {
    "ANA": "anaheim-ducks",
    "BOS": "boston-bruins",
    "BUF": "buffalo-sabres",
    "CGY": "calgary-flames",
    "CAR": "carolina-hurricanes",
    "CHI": "chicago-blackhawks",
    "COL": "colorado-avalanche",
    "CBJ": "columbus-blue-jackets",
    "DAL": "dallas-stars",
    "DET": "detroit-red-wings",
    "EDM": "edmonton-oilers",
    "FLA": "florida-panthers",
    "LAK": "los-angeles-kings",
    "MIN": "minnesota-wild",
    "MTL": "montreal-canadiens",
    "NSH": "nashville-predators",
    "NJD": "new-jersey-devils",
    "NYI": "new-york-islanders",
    "NYR": "new-york-rangers",
    "OTT": "ottawa-senators",
    "PHI": "philadelphia-flyers",
    "PIT": "pittsburgh-penguins",
    "SJS": "san-jose-sharks",
    "SEA": "seattle-kraken",
    "STL": "st-louis-blues",
    "TBL": "tampa-bay-lightning",
    "TOR": "toronto-maple-leafs",
    "UTA": "utah-mammoth",
    "VAN": "vancouver-canucks",
    "VGK": "vegas-golden-knights",
    "WSH": "washington-capitals",
    "WPG": "winnipeg-jets"
}

print(f"Scraping Puckpedia lineups pour {len(NHL_TEAMS)} équipes...\n")


def extract_column_players(column):
    """Extraire les joueurs d'une colonne"""
    players = []
    cards = column.select('a[href*="/player/"]')

    for i, card in enumerate(cards, 1):
        # Nom du joueur
        name_div = card.select_one('div[id^="player-name-"]')
        if not name_div:
            continue

        name = name_div.get_text(strip=True)

        # Numéro de chandail
        jersey_div = card.select_one('div[id^="jersey-number-"]')
        jersey_num = None
        if jersey_div:
            jersey_imgs = jersey_div.select('img[alt]')
            if jersey_imgs:
                jersey_num = ''.join([img.get('alt', '') for img in jersey_imgs])

        # Position
        position_spans = card.select('span')
        position = None
        for span in position_spans:
            text = span.get_text(strip=True)
            if text in ['C', 'LW', 'RW', 'D', 'L', 'R']:
                position = text
                break

        # Convertir L/R en LW/RW
        if position == 'L':
            position = 'LW'
        elif position == 'R':
            position = 'RW'

        players.append({
            'player_name': name,
            'position': position,
            'jersey_number': jersey_num,
            'order': i
        })

    return players


def parse_forwards(soup):
    """Parser les forwards (LW, C, RW)"""
    forward_section = soup.select_one('#nhl-forward-titles')

    if not forward_section:
        return []

    # Trouver le container suivant
    forward_container = forward_section.find_next_sibling('div')

    if not forward_container:
        return []

    columns = forward_container.find_all('div', recursive=False)

    if len(columns) < 3:
        return []

    all_forwards = []

    # Colonne 1: LW
    lw_players = extract_column_players(columns[0])
    for player in lw_players:
        all_forwards.append({
            'player_name': player['player_name'],
            'position': 'F',
            'jersey_number': player['jersey_number'],
            'line': player['order']
        })

    # Colonne 2: C
    c_players = extract_column_players(columns[1])
    for player in c_players:
        all_forwards.append({
            'player_name': player['player_name'],
            'position': 'F',
            'jersey_number': player['jersey_number'],
            'line': player['order']
        })

    # Colonne 3: RW
    rw_players = extract_column_players(columns[2])
    for player in rw_players:
        all_forwards.append({
            'player_name': player['player_name'],
            'position': 'F',
            'jersey_number': player['jersey_number'],
            'line': player['order']
        })

    return all_forwards


def parse_defense(soup):
    """Parser les défenseurs (LD, RD)"""
    defense_section = soup.select_one('#nhl-nonforward-titles')

    if not defense_section:
        return []

    # Trouver le container suivant
    defense_container = defense_section.find_next_sibling('div')

    if not defense_container:
        return []

    columns = defense_container.find_all('div', recursive=False)

    if len(columns) < 2:
        return []

    all_defense = []

    # Colonne 1: Left D
    ld_players = extract_column_players(columns[0])
    for player in ld_players:
        all_defense.append({
            'player_name': player['player_name'],
            'position': 'D',
            'jersey_number': player['jersey_number'],
            'line': player['order']
        })

    # Colonne 2: Right D
    rd_players = extract_column_players(columns[1])
    for player in rd_players:
        all_defense.append({
            'player_name': player['player_name'],
            'position': 'D',
            'jersey_number': player['jersey_number'],
            'line': player['order']
        })

    return all_defense


def parse_powerplay(soup):
    """Parser les unités de powerplay"""
    all_pp = []

    # Chercher tous les headers de powerplay
    pp_headers = soup.find_all('h3', class_='ml-2 self-start text-lg font-medium')

    for header in pp_headers:
        header_text = header.get_text(strip=True)

        # Vérifier si c'est un header de powerplay
        if 'Powerplay' in header_text:
            # Extraire le numéro (PP1, PP2, etc.)
            pp_num = header_text.replace('Powerplay', '').strip()
            pp_unit = f"PP{pp_num}"

            # Trouver le container de joueurs (le prochain div après le header)
            pp_container = header.find_next('div')

            if pp_container:
                # Chercher tous les joueurs dans ce container
                # IMPORTANT: Pour le PP, le nom est directement dans le <a>, pas dans un div
                player_links = pp_container.select('a[id^="player-name-"]')

                for link in player_links:
                    name = link.get_text(strip=True)

                    all_pp.append({
                        'player_name': name,
                        'pp_unit': pp_unit
                    })

    return all_pp


async def scrape_team_puckpedia(team_abbr, team_name, browser):
    """Scraper une équipe"""
    # Utiliser directement l'URL de l'iframe qui contient les lineups
    url = f"https://depth-charts.puckpedia.com/{team_abbr}"

    print(f"  Scraping: {team_abbr} - {url}")

    try:
        page = await browser.new_page()
        await page.goto(url, wait_until="domcontentloaded", timeout=5000)

        # Scroller vers le bas pour charger le powerplay
        await page.evaluate("window.scrollTo(0, document.body.scrollHeight)")
        await asyncio.sleep(2)  # Attendre le chargement du PP

        # Attendre que le contenu se charge
        try:
            await page.wait_for_selector('#nhl-forward-titles', timeout=2000)
        except:
            print(f"    ⚠ Section forwards non trouvée pour {team_abbr}")

        html = await page.content()
        await page.close()

        # Parser avec BeautifulSoup
        soup = BeautifulSoup(html, 'html.parser')

        forwards_data = parse_forwards(soup)
        defense_data = parse_defense(soup)
        powerplay_data = parse_powerplay(soup)

        # Créer un dict pour merger les données PP
        pp_dict = {}
        for pp_player in powerplay_data:
            name = pp_player['player_name']
            if name not in pp_dict:
                pp_dict[name] = []
            pp_dict[name].append(pp_player['pp_unit'])

        all_players = forwards_data + defense_data

        # Ajouter les colonnes team, pp_unit, source et merger PP
        for player in all_players:
            player['team'] = team_abbr
            player['source'] = 'puckpedia'

            # Ajouter pp_unit si le joueur est dans une unité PP
            if player['player_name'] in pp_dict:
                # Joindre toutes les unités PP (PP1, PP2, etc.)
                player['pp_unit'] = ', '.join(pp_dict[player['player_name']])
            else:
                player['pp_unit'] = None

        # Filtrer les joueurs sans nom
        all_players = [p for p in all_players if p['player_name']]

        print(f"    ✓ {len(all_players)} joueurs trouvés")

        return all_players

    except Exception as e:
        print(f"    ✗ Erreur: {e}")
        return []

    finally:
        time.sleep(1)  # Délai entre requêtes


# Test pour une équipe
async def test_single_team():
    async with async_playwright() as p:
        browser = await p.chromium.launch(
            headless=False,  # Mode visible pour debug
            args=['--disable-blink-features=AutomationControlled']
        )
        test_result = await scrape_team_puckpedia("MTL", "montreal-canadiens", browser)
        await browser.close()
        print(f"\nTest MTL: {len(test_result)} joueurs")
        if test_result:
            df = pd.DataFrame(test_result)
            print(df.to_string())  # Afficher tous les joueurs
        return test_result

# Pour exécuter dans Jupyter: await test_single_team()

async def main():
    """Fonction principale"""
    all_lineups = []

    async with async_playwright() as p:
        # Lancer le navigateur en mode headless
        browser = await p.chromium.launch(
            headless=True,
            args=['--disable-blink-features=AutomationControlled']
        )

        # Scraper toutes les équipes
        for team_abbr, team_name in NHL_TEAMS.items():
            players = await scrape_team_puckpedia(team_abbr, team_name, browser)
            all_lineups.extend(players)

        await browser.close()

    # Créer DataFrame
    df = pd.DataFrame(all_lineups)

    # Résumé
    print("\n" + "="*40)
    print("RÉSUMÉ DU SCRAPING")
    print("="*40 + "\n")

    if len(df) > 0:
        print(f"Joueurs scrapés: {len(df)}")
        print(f"Équipes: {df['team'].nunique()}\n")

        summary = df.groupby('team').agg(
            n_players=('player_name', 'count'),
            n_forwards=('position', lambda x: (x == 'F').sum()),
            n_defense=('position', lambda x: (x == 'D').sum()),
            n_pp=('pp_unit', lambda x: x.notna().sum())
        )

        print(summary)
    else:
        print("⚠ Aucun joueur scrapé\n")

    # Sauvegarder
    df.to_csv(OUTPUT_FILE, index=False)
    print(f"\n✓ Données sauvegardées: {OUTPUT_FILE}")

    # Aperçu
    if len(df) > 0:
        print("\nAperçu (20 premières lignes):")
        print(df.head(20))

    print("\n" + "="*40)
    print("TERMINÉ")
    print("="*40 + "\n")
    print(f"✓ Données sauvegardées: {OUTPUT_FILE}")
    print(f"✓ {len(df)} joueurs de {df['team'].nunique()} équipes scrapés\n")


if __name__ == "__main__":
    # Scraper toutes les équipes
    asyncio.run(main())

    # Pour tester une seule équipe, utilisez:
    # asyncio.run(test_single_team())
